mod llvm;
mod context;

use std::collections::HashMap;
use anyhow::anyhow;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::IntPredicate;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use crate::checker::Substitution;
use crate::parser::ast::{AssignStatement, BinaryOp, Block, ControlStatement, DType, Either, Expression, Function, IfStatement, LoopStatement, Program, ReturnStatement, Spanned, Statement};
use crate::parser::ast::Statement::If;

macro_rules! build_int_compare {
    ($this:ident, $lhs:ident, $rhs:ident, $predicate:path) => {
        match ($lhs.get_type(), $rhs.get_type()) {
            (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                $this.ir_builder.build_int_compare($predicate, $lhs.into_int_value(), $rhs.into_int_value(), "").unwrap().into()
            }
            _ => unreachable!(),
        }
    };
}

macro_rules! build_int_arithmetic {
    ($this:ident, $lhs:ident, $rhs:ident, $method:ident) => {
        match ($lhs.get_type(), $rhs.get_type()) {
            (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                $this.ir_builder.$method($lhs.into_int_value(), $rhs.into_int_value(), "").unwrap().into()
            }
            _ => unreachable!(),
        }
    };
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct Variable<'ctx> {
    pub(crate) typ: BasicTypeEnum<'ctx>,
    pub(crate) addr: PointerValue<'ctx>,
}

pub(crate) struct LoopContext<'ctx> {
    continue_block: BasicBlock<'ctx>,
    break_block: BasicBlock<'ctx>,
}

pub(crate) struct Compiler<'ctx> {
    symbols: HashMap<String, Variable<'ctx>>,
    ir_builder: Builder<'ctx>,
    pub(crate) module: Module<'ctx>,
    context: &'ctx inkwell::context::Context,
    substitution: Substitution,
    current_function: Option<FunctionValue<'ctx>>,
    loop_context: Vec<LoopContext<'ctx>>,
    functions: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> Compiler<'ctx> {
    pub(crate) fn new(
        context: &'ctx inkwell::context::Context,
        substitution: Substitution,
    ) -> Self {
        Self {
            symbols: HashMap::new(),
            ir_builder: context.create_builder(),
            module: context.create_module("main"),
            context,
            current_function: None,
            substitution,
            loop_context: Vec::new(),
            functions: HashMap::new(),
        }
    }

    pub(crate) fn compile_program(&mut self, program: &Program) {
        for function in &program.functions {
            let name = (*function.name).clone();
            self.compile_function(function);
        }
    }

    fn compile_assign_statement(&mut self, assign: &Spanned<AssignStatement>) {
        let typ = self.get_llvm_type(assign.typ.type_idx());
        let value = self.ir_builder.build_alloca(typ, &assign.identifier).unwrap();

        let expression = self.compile_expression(&assign.expression);
        self.ir_builder.build_store(value, expression).unwrap();
        self.symbols.insert((*assign.identifier).clone(), Variable {
            typ,
            addr: value,
        });
    }

    fn compile_return_statement(&mut self, stat: &Spanned<ReturnStatement>) {
        let value = self.compile_expression(&stat.expression);
        self.ir_builder.build_return(Some(&value.into_int_value())).unwrap();
    }


    fn compile_if_statement(&mut self, stat: &Spanned<IfStatement>) {
        let function = self.ir_builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let IfStatement {
            if_block,
            else_if_block,
            else_block,
        } = &stat.node;

        // Create the final merge block first. All branches will eventually jump here.
        let merge_block = self.context.append_basic_block(function, "if.merge");

        self.ir_builder.position_at_end(merge_block);
        let phi_node = self.ir_builder.build_phi(self.context.i32_type(), "if.result").unwrap();

        // Create the first block in `if` branch.
        let then_block = self.context.append_basic_block(function, "if.then");

        // Choose the next block.
        let first_else_dest = if !else_if_block.is_empty() {
            self.context.append_basic_block(function, "elseif.cond")
        } else if else_block.is_some() {
            self.context.append_basic_block(function, "else.final")
        } else {
            merge_block
        };

        let entry_block = self.ir_builder.get_insert_block().unwrap();
        self.ir_builder.position_at_end(entry_block);

        let condition = self.compile_expression(&if_block.condition).into_int_value();
        self.ir_builder.build_conditional_branch(condition, then_block, first_else_dest).unwrap();

        self.ir_builder.position_at_end(then_block);
        self.compile_block(&if_block.block);
        phi_node.add_incoming(&[(&self.context.i32_type().const_int(1, false), self.ir_builder.get_insert_block().unwrap())]);
        self.ir_builder.build_unconditional_branch(merge_block).unwrap();

        let mut current_cond_block = first_else_dest;
        for (i, else_if) in else_if_block.iter().enumerate() {
            self.ir_builder.position_at_end(current_cond_block);

            let condition = self.compile_expression(&else_if.condition).into_int_value();
            let then_block = self.context.append_basic_block(function, &format!("elseif.then.{}", i));

            let next_else_dest = if i < else_if_block.len() - 1 {
                self.context.append_basic_block(function, &format!("elseif.then.{}", i + 1))
            } else if else_block.is_some() {
                self.context.append_basic_block(function, "else.final")
            } else {
                merge_block
            };

            self.ir_builder.build_conditional_branch(condition, then_block, next_else_dest).unwrap();
            self.ir_builder.position_at_end(then_block);
            phi_node.add_incoming(&[(&self.context.i32_type().const_int(1, false), self.ir_builder.get_insert_block().unwrap())]);
            self.ir_builder.build_unconditional_branch(merge_block).unwrap();

            current_cond_block = next_else_dest;
        }

        if let Some(else_b) = else_block {
            self.ir_builder.position_at_end(current_cond_block);
            phi_node.add_incoming(&[(&self.context.i32_type().const_int(1, false), self.ir_builder.get_insert_block().unwrap())]);
            self.ir_builder.build_unconditional_branch(merge_block).unwrap();
        }

        self.ir_builder.position_at_end(merge_block);
    }

    fn compile_loop_statement(&mut self, stat: &Spanned<LoopStatement>) {
        let function = self.ir_builder.get_insert_block().unwrap().get_parent().unwrap();

        let loop_body = self.context.append_basic_block(function, "loop.body");
        let after_loop = self.context.append_basic_block(function, "loop.exit");

        self.loop_context.push(LoopContext {
            continue_block: loop_body,
            break_block: after_loop,
        });
        self.ir_builder.build_unconditional_branch(loop_body).unwrap();
        self.ir_builder.position_at_end(loop_body);

        self.compile_block(&stat.block);

        // Check whether there are terminator instructions in this block (such as break).
        if self.ir_builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.ir_builder.build_unconditional_branch(loop_body).unwrap();
        }
        self.loop_context.pop();
        self.ir_builder.position_at_end(after_loop);
    }

    fn compile_break(&self) {
        if let Some(loop_ctx) = self.loop_context.last() {
            self.ir_builder.build_unconditional_branch(loop_ctx.break_block).unwrap();
        }
    }

    fn compile_continue(&self) {
        if let Some(loop_ctx) = self.loop_context.last() {
            self.ir_builder.build_unconditional_branch(loop_ctx.continue_block).unwrap();
        }
    }

    fn compile_statement(&mut self, statement: &Spanned<Statement>) {
        match &statement.node {
            Statement::Assign(assign) => self.compile_assign_statement(assign),
            Statement::Expression(_) => {}
            Statement::Return(return_stat) => self.compile_return_statement(return_stat),
            Statement::Loop(stat) => self.compile_loop_statement(stat),
            Statement::Control(stat) => match stat.node {
                ControlStatement::Continue => self.compile_continue(),
                ControlStatement::Break => self.compile_break(),
            }
            _ => {}
        }
    }

    fn function_metadata<'a>(&mut self, function_ast: &'a Spanned<Function>) -> (String, Vec<String>, Vec<BasicTypeEnum<'ctx>>, BasicTypeEnum<'ctx>, &'a Block) {
        let param_list = &function_ast.param_list.node;

        let (args_names, args_types) = param_list
            .params
            .iter()
            .map(|param| {
                let typ = self.dtype_to_basic_llvm_type(param.typ.typ);
                ((*param.name).clone(), typ)
            })
            .unzip::<String, BasicTypeEnum, Vec<String>, Vec<BasicTypeEnum>>();
        let return_type = self.dtype_to_basic_llvm_type(function_ast.return_type.typ);
        ((*function_ast.name).clone(), args_names, args_types, return_type, &function_ast.block)
    }

    fn compile_function_params(
        &mut self,
        function: FunctionValue,
        arg_names: Vec<String>,
        args_types: Vec<BasicTypeEnum<'ctx>>
    ) -> anyhow::Result<()> {
        for (i, name) in arg_names.into_iter().enumerate() {
            let arg_val = function.get_nth_param(i as u32).unwrap();
            arg_val.set_name(&name);

            let arg_ptr = self.ir_builder.build_alloca(args_types[i], &name)?;
            self.ir_builder.build_store(arg_ptr, arg_val)?;
            self.symbols.insert(name, Variable {
                typ: args_types[i],
                addr: arg_ptr,
            });
        }
        Ok(())
    }

    fn compile_function(&mut self, function_ast: &Spanned<Function>) -> anyhow::Result<()> {
        let (
            function_name,
            args_names,
            args_types,
            return_type,
            block
        ) = self.function_metadata(function_ast);

        let fn_args_types = args_types
            .iter()
            .cloned()
            .map(|typ| typ.into())
            .collect::<Vec<_>>();
        let fn_type = return_type.fn_type(&fn_args_types, false);

        let function = self.module.add_function(&function_name, fn_type, None);
        let entry_block = self.context.append_basic_block(function, "entry");

        self.ir_builder.position_at_end(entry_block);

        self.with_new_function_scope(|this| {
            this.compile_function_params(
                function,
                args_names,
                args_types,
            ).unwrap();
            this.compile_block(block);
        });

        if function.verify(true) {
            self.functions.insert(function_name, function);
            return Ok(());
        }

        self.module.print_to_string();
        Err(anyhow!("Failed to compile function"))
    }

    fn compile_block(&mut self, block: &Block) {
        block.statements
            .iter()
            .for_each(|statement| {
                match statement {
                    Either::Left(statement) => self.compile_statement(statement),
                    Either::Right(block) => self.compile_block(block),
                }
            });
    }

    fn compile_expression(&mut self, expression: &Expression) -> BasicValueEnum<'ctx> {
        match expression {
            Expression::NumberLiteral {
                value, typ,
            } => {
                let int_typ = self.get_int_llvm_type(typ.type_idx());
                int_typ.const_int(value.node as u64, false).into()
            }
            Expression::BooleanLiteral(value) => self.context.bool_type().const_int(value.node as u64, false).into(),
            Expression::Variable { name, .. } => {
                let var = &self.symbols[&name.node];
                self.ir_builder.build_load(var.typ, self.variable(&name), "").unwrap()
            }
            x @ Expression::BinaryOp { .. } => self.compile_binary_expression(x),
            Expression::Invoke(invoke) => {
                let function_name = (*invoke.identifier).clone();
                let function = self.functions[&function_name];

                let mut args = Vec::<BasicMetadataValueEnum>::new();
                for expr in &invoke.param_list.expressions {
                    let arg_val = self.compile_expression(expr);
                    args.push(arg_val.into());
                }

                let call_site_value = self.ir_builder.build_call(
                    function,
                    &args,
                    "tmp_call"
                ).unwrap();

                call_site_value.try_as_basic_value().left().unwrap()
            },
        }
    }

    fn compile_binary_expression(&mut self, expression: &Expression) -> BasicValueEnum<'ctx> {
        match expression {
            Expression::BinaryOp {
                op, lhs, rhs
            } => {
                let lhs = self.compile_expression(&lhs.node);
                let rhs = self.compile_expression(&rhs.node);

                match op {
                    BinaryOp::Add => build_int_arithmetic!(self, lhs, rhs, build_int_add),
                    BinaryOp::Subtract => build_int_arithmetic!(self, lhs, rhs, build_int_sub),
                    BinaryOp::Multiply => build_int_arithmetic!(self, lhs, rhs, build_int_mul),
                    BinaryOp::Divide => build_int_arithmetic!(self, lhs, rhs, build_int_signed_div),
                    // TODO: Implement short-circuit evaluation for logical AND/OR
                    BinaryOp::And => build_int_arithmetic!(self, lhs, rhs, build_and),
                    BinaryOp::Or => build_int_arithmetic!(self, lhs, rhs, build_or),
                    BinaryOp::Equal => build_int_compare!(self, lhs, rhs, IntPredicate::EQ),
                    BinaryOp::NotEqual => build_int_compare!(self, lhs, rhs, IntPredicate::NE),
                    BinaryOp::Greater => build_int_compare!(self, lhs, rhs, IntPredicate::SGT),
                    BinaryOp::GreaterOrEqual => build_int_compare!(self, lhs, rhs, IntPredicate::SGE),
                    BinaryOp::Less => build_int_compare!(self, lhs, rhs, IntPredicate::SLT),
                    BinaryOp::LessOrEqual => build_int_compare!(self, lhs, rhs, IntPredicate::SLE),
                    _ => todo!()
                }
            }
            _ => unreachable!()
        }
    }

    fn variable(&self, name: &str) -> PointerValue<'ctx> {
        self.symbols[name].addr
    }

    fn dtype_to_basic_llvm_type(&self, dtype: DType) -> BasicTypeEnum<'ctx> {
        match dtype {
            DType::I8 => self.context.i8_type().into(),
            DType::I16 => self.context.i16_type().into(),
            DType::I32 => self.context.i32_type().into(),
            DType::I64 => self.context.i64_type().into(),
            DType::U8 => self.context.i8_type().into(),
            DType::U16 => self.context.i16_type().into(),
            DType::U32 => self.context.i32_type().into(),
            DType::U64 => self.context.i64_type().into(),
            DType::Bool => self.context.bool_type().into(),
            _ => unreachable!(),
        }
    }

    fn int_dtype_to_int_llvm_type(&self, dtype: DType) -> IntType<'ctx> {
        match dtype {
            DType::I8 => self.context.i8_type().into(),
            DType::I16 => self.context.i16_type().into(),
            DType::I32 => self.context.i32_type().into(),
            DType::I64 => self.context.i64_type().into(),
            DType::U8 => self.context.i8_type().into(),
            DType::U16 => self.context.i16_type().into(),
            DType::U32 => self.context.i32_type().into(),
            DType::U64 => self.context.i64_type().into(),
            _ => unreachable!(),
        }
    }

    fn get_type(&mut self, idx: u64) -> DType {
        self.substitution.find(idx)
    }

    fn get_llvm_type(&mut self, idx: u64) -> BasicTypeEnum<'ctx> {
        let typ = self.get_type(idx);
        self.dtype_to_basic_llvm_type(typ)
    }

    fn get_int_llvm_type(&mut self, idx: u64) -> IntType<'ctx> {
        let typ = self.get_type(idx);
        self.int_dtype_to_int_llvm_type(typ)
    }

    fn with_new_function_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let old_symbols = self.symbols.clone();
        self.symbols.clear();

        let result = f(self);

        self.symbols = old_symbols;
        result
    }

    fn with_new_block_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let old_symbols = self.symbols.clone();

        let result = f(self);
        self.symbols = old_symbols;
        result
    }
}