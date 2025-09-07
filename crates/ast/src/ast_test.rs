use token::token;

use crate::ast::{Identifier, LetStatement, Program};

#[test]
fn test_to_string() {
    let program = Program {
        statements: vec![Box::new(LetStatement {
            token: token::Token {
                type_: token::LET,
                literal: "let".to_string(),
            },
            name: Some(Identifier {
                token: token::Token {
                    type_: token::IDENT,
                    literal: "myVar".to_string(),
                },
                value: "myVar".to_string(),
            }),
            value: Some(Box::new(Identifier {
                token: token::Token {
                    type_: token::IDENT,
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
