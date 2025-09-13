use token::token::{Token, TokenType};

use crate::ast::{Identifier, LetStatement, Program, StatementTypes};

#[test]
fn test_to_string() {
    let program = Program {
        statements: vec![StatementTypes::Let(LetStatement {
            token: Token {
                type_: TokenType::LET,
                literal: "let".to_string(),
            },
            name: Some(Identifier {
                token: Token {
                    type_: TokenType::IDENT,
                    literal: "myVar".to_string(),
                },
                value: "myVar".to_string(),
            }),
            value: Some(Box::new(Identifier {
                token: Token {
                    type_: TokenType::IDENT,
                    literal: "anotherVar".to_string(),
                },
                value: "anotherVar".to_string(),
            })),
        })],
    };

    if program.to_string() != "let myVar = anotherVar;" {
        panic!("program.to_string() wrong. got={}", program.to_string());
    }
}
