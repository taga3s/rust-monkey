use std::io::{self, Write};

use lexer::lexer;
use token::token;

fn start() -> io::Result<()> {
    let mut input = String::new();

    loop {
        input.clear();

        print!(">> ");
        io::stdout().flush()?; // To ensure that the prompt is printed immediately
        io::stdin().read_line(&mut input)?;

        if input == "/exit" {
            break;
        }

        let mut lexer = lexer::new(input.clone());

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
