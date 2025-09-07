use std::io::{self, Write};

use lexer::lexer;
use token::token;

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

        let mut lexer = lexer::new(trimmed.to_string());

        loop {
            let tok = lexer.next_token();

            if tok.type_ == token::EOF {
                break;
            }

            println!("{:?}", tok);
        }
    }

    Ok(())
}

fn main() {
    println!("Feel free to type in commands\n");

    if let Err(e) = start() {
        eprintln!("Error: {}", e);
    }
}
