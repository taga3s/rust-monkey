use std::{env, fs::File, io::Read, path::Path};

use evaluator::evaluator;
use lexer::lexer::Lexer;
use object::{environment, object::ObjectTypes};
use parser::parser::Parser;

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

    let mut file = match File::open(&path) {
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

    run(&s);
}

fn run(input: &str) {
    let mut env = environment::new_environment();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    if parser.errors().len() != 0 {
        print_parse_errors(parser.errors());
        return;
    }

    let evaluated = evaluator::eval(&program, &mut env);
    if ObjectTypes::Null(object::object::Null {}) == evaluated {
        return;
    }
    println!("{}", evaluated.inspect());
}

fn print_parse_errors(errors: &Vec<String>) {
    for msg in errors {
        eprintln!("Error: {}", msg);
    }
}
