use std::any::Any;
use std::collections::HashMap;
use anyhow::anyhow;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use crate::checker::Substitution;
use crate::parser::ast::{BinaryOp, Block, DType, Expression, Function, MaybeNull, Program, Statement};


#[derive(Debug, Copy, Clone)]
pub(crate) struct Variable<'ctx> {
    pub(crate) typ: BasicTypeEnum<'ctx>,
    pub(crate) addr: PointerValue<'ctx>,
}

pub(crate) struct Compiler<'ctx> {
    symbols: HashMap<String, Variable<'ctx>>,
    ir_builder: Builder<'ctx>,
    pub(crate) module: Module<'ctx>,
    context: &'ctx inkwell::context::Context,
    substitution: Substitution,
    current_function: Option<FunctionValue<'ctx>>,
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
        }
    }

    pub(crate) fn compile_program(&mut self, program: Program) {
        program.functions
            .into_iter()
            .for_each(|function| {
                self.compile_function(function).unwrap();
            });
    }

    fn compile_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Assign(assign) => {
                let typ = self.get_llvm_type(assign.typ.type_idx());
                let value = self.ir_builder.build_alloca(typ, &assign.identifier).unwrap();
                let expression = self.compile_expression(assign.expression);
                self.ir_builder.build_store(value, expression).unwrap();
                self.symbols.insert(assign.identifier, Variable {
                    typ,
                    addr: value,
                });
            }
            Statement::Expression(_) => {}
            Statement::Return(return_stat) => {
                let value = self.compile_expression(return_stat.expression);
                self.ir_builder.build_return(Some(&value.into_int_value())).unwrap();
            }
        }
    }

    fn function_metadata(&mut self, function_ast: Function) -> (String, Vec<String>, Vec<BasicTypeEnum<'ctx>>, BasicTypeEnum<'ctx>, Block) {
        let (args_names, args_types) = function_ast
            .param_list
            .params
            .into_iter()
            .map(|param| (param.name, self.dtype_to_basic_llvm_type(param.typ.typ)))
            .unzip::<String, BasicTypeEnum, Vec<String>, Vec<BasicTypeEnum>>();
        let return_type = self.dtype_to_basic_llvm_type(function_ast.return_type.typ);
        (function_ast.name, args_names, args_types, return_type, function_ast.block)
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

    fn compile_function(&mut self, function_ast: Function) -> anyhow::Result<FunctionValue> {
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
            return Ok(function);
        }

        self.module.print_to_string();
        Err(anyhow!("Failed to compile function"))
    }

    fn compile_block(&mut self, block: Block) {
        block.statements
            .into_iter()
            .for_each(|statement| self.compile_statement(statement));
    }

    fn compile_expression(&mut self, expression: Expression) -> BasicValueEnum<'ctx> {
        match expression {
            Expression::NumberLiteral {
                value, typ,
            } => {
                let int_typ = self.get_int_llvm_type(typ.type_idx());
                int_typ.const_int(value as u64, false).into()
            }
            Expression::BooleanLiteral(value) => self.context.bool_type().const_int(value as u64, false).into(),
            Expression::Variable { name, .. } => {
                let var = &self.symbols[&name];
                self.ir_builder.build_load(var.typ, self.variable(&name), "").unwrap()
            }
            x @ Expression::BinaryOp { .. } => self.compile_binary_expression(x),
        }
    }

    fn compile_binary_expression(&mut self, expression: Expression) -> BasicValueEnum<'ctx> {
        match expression {
            Expression::BinaryOp {
                op, lhs, rhs
            } => {
                let lhs = self.compile_expression(*lhs);
                let rhs = self.compile_expression(*rhs);

                match op {
                    BinaryOp::Add => {
                        match (lhs.get_type(), rhs.get_type()) {
                            (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                                self.ir_builder.build_int_add(lhs.into_int_value(), rhs.into_int_value(), "").unwrap().into()
                            }
                            _ => unreachable!()
                        }
                    }
                    BinaryOp::Subtract => {
                        match (lhs.get_type(), rhs.get_type()) {
                            (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                                self.ir_builder.build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "").unwrap().into()
                            }
                            _ => unreachable!()
                        }
                    }
                    BinaryOp::Multiply => {
                        match (lhs.get_type(), rhs.get_type()) {
                            (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                                self.ir_builder.build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "").unwrap().into()
                            }
                            _ => unreachable!()
                        }
                    }
                    BinaryOp::Divide => {
                        match (lhs.get_type(), rhs.get_type()) {
                            (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_)) => {
                                self.ir_builder.build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "").unwrap().into()
                            }
                            _ => unreachable!()
                        }
                    }
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