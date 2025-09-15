use std::io::{self, Write};

use ::lexer::lexer::Lexer;
use ast::ast::Node;
use parser::parser::Parser;

fn start() -> io::Result<()> {
    let mut input = String::new();

    loop {
        input.clear();

        print!(">> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut input)?;

        let trimmed = input.trim();

        if trimmed == "/exit" {
            break;
        }

        let lexer = Lexer::new(trimmed.to_string());
        let mut parser = Parser::new(lexer);
        let program = match parser.parse_program() {
            Node::Program(p) => p,
            _ => {
                panic!("parser.parse_program() did not return Program.");
            }
        };
        if parser.errors().len() != 0 {
            print_parse_errors(parser.errors());
            continue;
        }

        println!("{}", program.to_string());
    }

    Ok(())
}

fn print_parse_errors(errors: &Vec<String>) {
    for msg in errors {
        eprintln!("\t{}", msg);
    }
}

fn main() {
    println!("Feel free to type in commands\n");

    if let Err(e) = start() {
        eprintln!("Error: {}", e);
    }
}
