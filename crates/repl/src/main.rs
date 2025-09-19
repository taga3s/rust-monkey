use std::io::{self, Write};

use ::lexer::lexer::Lexer;
use evaluator::evaluator;
use object::environment;
use parser::parser::Parser;

fn start() -> io::Result<()> {
    let mut input = String::new();
    let mut env = environment::new_environment();

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
        let program = parser.parse_program();
        if parser.errors().len() != 0 {
            print_parse_errors(parser.errors());
            continue;
        }

        let evaluated = evaluator::eval(&program, &mut env);
        println!("{}", evaluated.inspect());
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
