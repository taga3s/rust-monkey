use std::{env, fs::File, io::Read, path::Path};

use runner::runner::run;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: just run <FILE_PATH>");
        std::process::exit(1);
    }

    let file_path = &args[1];
    if !file_path.ends_with(".mon") {
        eprintln!("Error: The file must have a \".mon\" extension.");
        std::process::exit(1);
    }

    let path = Path::new(file_path);
    let display = path.display();
    let mut file = match File::open(path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("couldn't open {}: {}", display, err);
            std::process::exit(1);
        }
    };

    let mut buf = String::new();
    match file.read_to_string(&mut buf) {
        Ok(_) => {
            if let Some(result) = run(&buf) {
                println!("{}", result);
            }
        }
        Err(err) => {
            eprintln!("couldn't read {}: {}", display, err);
            std::process::exit(1)
        }
    }
}
