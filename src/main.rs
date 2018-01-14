use std::process::exit;

extern crate cc;
use cc::Config;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let config = Config::new(&args).unwrap_or_else(|err| {
        println!("Problem parsing arguments: {}", err);
        exit(1);
    });
    if let Err(e) = cc::run(config) {
        println!("Compilation error: {}", e);
        exit(1);
    }
}
