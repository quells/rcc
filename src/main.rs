use std::process::exit;

extern crate clap;
extern crate rcc;

const PROGRAM_NAME: &'static str = "rcc";
const VERSION_NUMBER: &'static str = "0.3.1";
const PROGRAM_AUTHOR: &'static str = "Kai Wells <support@kaiwells.me>";
const PROGRAM_DESCRIPTION: &'static str = "A very simple C compiler";

fn main() {
    let matches = clap::App::new(PROGRAM_NAME)
        .version(VERSION_NUMBER)
        .author(PROGRAM_AUTHOR)
        .about(PROGRAM_DESCRIPTION)
        .args_from_usage(
            "<INPUT>      'Source file to compile'
            [-o <OUTPUT>] 'Destination file for compilation'
            -v            'Print debug messages'"
        )
        .get_matches();
    
    let input_file = matches.value_of("INPUT").unwrap_or_else(|| {
        println!("Missing source file");
        exit(1);
    });
    let default_output_file = input_file.to_owned() + ".s";
    let output_file = matches.value_of("OUTPUT").unwrap_or(&default_output_file);
    let verbose = matches.is_present("v");

    if let Err(e) = rcc::run(&input_file, &output_file, verbose) {
        eprintln!("Compilation error: {}", e);
        exit(1);
    }
}
