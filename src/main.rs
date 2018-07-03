use std::process::exit;

extern crate rcc;
use rcc::config::Config;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let config = Config::new(&args).unwrap_or_else(|err| {
        println!("Problem parsing arguments: {}", err);
        exit(1);
    });
    if let Err(e) = rcc::run(config) {
        println!("Compilation error: {}", e);
        exit(1);
    }
}
