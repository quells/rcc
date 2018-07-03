use std::env;
use std::path::Path;
use std::process::exit;

fn usage_info() {
    let args: Vec<String> = env::args().collect();
    let exe_filename: String = args[0].clone();
    println!("Usage:");
    println!("{} src.c [-o dest.s]", exe_filename);
    println!("  Compile C file into assembly");
    println!("  src.c:  Source C file");
    println!("  dest.c: Destination assembly file");
    println!("");
    println!("{} -h", exe_filename);
    println!("  Display this message");
    exit(0)
}

pub struct Config {
    pub src_filename: String,
    pub dest_filename: String,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        // expecting: cc src.c -o dest.s
        // if: cc src.c, then: cc src.c -o src.s

        match args.len() {
            0 => Err("how did you get here?"),
            1 => {
                println!("Error: no input file provided");
                usage_info();
                Err("") // program should exit before here, but this makes the compiler happy
            },
            2 => {
                let src_filename = args[1].clone();
                if src_filename == "-h" {
                    usage_info()
                }
                let dest_filename = {
                    let src_path = Path::new(&src_filename);
                    src_path.with_extension("s").to_string_lossy().into_owned()
                };
                Ok(Config { src_filename, dest_filename })
            },
            4 => {
                let src_filename = args[1].clone();
                let flag = args[2].clone();
                if flag != "-o" {
                    return Err("unexpected flag in second argument")
                }
                let dest_filename = args[3].clone();
                Ok(Config { src_filename, dest_filename })
            },
            _ => {
                println!("Error: unexpected number of arguments");
                usage_info();
                Err("") // program should exit before here, but this makes the compiler happy
            },
        }
    }
}
