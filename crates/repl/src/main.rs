use core::panic;
use std::io::{self, Write};

use ::lexer::lexer::Lexer;
use evaluator::evaluator;
use object::environment::Environment;
use parser::parser::Parser;

fn start() -> io::Result<()> {
    let mut input = String::new();
    let env = Environment::new();

    loop {
        input.clear();

        print!(">> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut input)?;

        let trimmed = input.trim();

        if trimmed.starts_with('/') {
            match trimmed {
                "/quit" => {
                    println!("Goodbye!");
                    break;
                }
                _ => {
                    println!("Unknown command: {}", trimmed);
                    continue;
                }
            }
        }

        if trimmed.is_empty() {
            continue;
        }

        let lexer = Lexer::new(trimmed);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if parser.errors().len() != 0 {
            print_parse_errors(parser.errors());
            continue;
        }

        let evaluated = evaluator::eval(&program, env.clone());
        println!("{}", evaluated.inspect());
    }

    Ok(())
}

fn print_parse_errors(errors: &Vec<String>) {
    for msg in errors {
        eprintln!("Error: {}", msg);
    }
}

fn main() {
    println!("Welcome to rust-monkey REPL! Feel free to type in commands.\nType /quit to exit.");

    if let Err(e) = start() {
        panic!("Error: {}", e);
    }
}
