use std::{env, fs::File, io::Read, path::Path};

use runner::runner::run;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: just run <FILE_PATH>");
        std::process::exit(1);
    }

    let file_path = &args[1];
    // if !file_path.ends_with(".monkey") {
    //     eprintln!("Error: The file must have a .monkey extension.");
    //     std::process::exit(1);
    // }
    let path = Path::new(file_path);
    let display = path.display();

    let mut file = match File::open(path) {
        Err(why) => {
            eprintln!("couldn't open {}: {}", display, why);
            std::process::exit(1);
        }
        Ok(file) => file,
    };

    let mut s = String::new();
    if let Err(err) = file.read_to_string(&mut s) {
        eprintln!("couldn't read {}: {}", display, err);
        std::process::exit(1);
    }

    let out = run(&s);
    if let Some(result) = out {
        println!("{}", result);
    }
}
