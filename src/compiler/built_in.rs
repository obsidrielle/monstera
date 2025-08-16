#[unsafe(no_mangle)]
pub extern "C" fn print(value: i32) {
    println!("{}", value);
}

