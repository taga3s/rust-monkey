use ast::ast::{
    ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Node, ReturnStatement, Statement,
};
use lexer::lexer::new;

use crate::parser::Parser;

fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();
    if errors.len() == 0 {
        return;
    }

    eprintln!("parser has {} errors", errors.len());
    for err in errors {
        eprintln!("parser error: {}", err);
    }
}

#[test]
fn test_let_statements() {
    let input = r#"
      let x = 5;
      let y = 10;
      let foobar = 838383;
    "#;

    let lexer = new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    check_parser_errors(&parser);

    if program.is_none() {
        panic!("parse_program() returned None");
    }
    let p = program.unwrap();
    if p.statements.len() != 3 {
        panic!(
            "program.statements does not contain 3 statements. got={}",
            p.statements.len()
        );
    }

    let tests = vec!["x", "y", "foobar"];

    for (i, ident) in tests.iter().enumerate() {
        let stmt = &p.statements[i];
        if !test_let_statement(stmt, ident) {
            return;
        }
    }

    fn test_let_statement(stmt: &Box<dyn Statement>, ident: &str) -> bool {
        if stmt.token_literal() != "let" {
            panic!("stmt.token_literal not 'let'. got={}", stmt.token_literal());
        }

        let let_stmt = stmt.as_any().downcast_ref::<LetStatement>();

        let let_stmt = let_stmt.unwrap_or_else(|| {
            panic!("stmt is not LetStatement.");
        });

        if let_stmt.name.as_ref().unwrap().value != ident {
            panic!(
                "let_stmt.name.value not '{}'. got={}",
                ident,
                let_stmt.name.as_ref().unwrap().value
            );
        }

        if let_stmt.name.as_ref().unwrap().token_literal() != *ident {
            panic!(
                "let_stmt.name.token_literal() not '{}'. got={}",
                ident,
                let_stmt.name.as_ref().unwrap().token_literal()
            );
        }

        true
    }
}

#[test]
fn test_return_statements() {
    let input = r#"
      return 5;
      return 10;
      return 838383;
    "#;

    let lexer = new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    check_parser_errors(&parser);

    if program.is_none() {
        panic!("parse_program() returned None");
    }
    let p = program.unwrap();

    if p.statements.len() != 3 {
        panic!(
            "program.statements does not contain 3 statements. got={}",
            p.statements.len()
        );
    }

    for stmt in p.statements.iter() {
        let return_stmt = stmt.as_any().downcast_ref::<ReturnStatement>();

        let return_stmt = return_stmt.unwrap_or_else(|| {
            panic!("stmt is not ReturnStatement.");
        });

        if return_stmt.token_literal() != "return" {
            panic!(
                "return_stmt.token_literal() not 'return'. got={}",
                return_stmt.token_literal()
            );
        }
    }
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;";

    let lexer = new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    check_parser_errors(&parser);

    if program.is_none() {
        panic!("parse_program() returned None");
    }
    let p = program.unwrap();

    if p.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statement. got={}",
            p.statements.len()
        );
    }

    let stmt = match p.statements[0]
        .as_any()
        .downcast_ref::<ExpressionStatement>()
    {
        Some(stmt) => stmt,
        None => panic!("stmt is not ExpressionStatement."),
    };

    let ident = match stmt
        .expression
        .as_ref()
        .unwrap()
        .as_any()
        .downcast_ref::<Identifier>()
    {
        Some(ident) => ident,
        None => panic!("stmt.expression is not Identifier."),
    };

    if ident.value != "foobar" {
        panic!("ident.value is not 'foobar'. got={}", ident.value);
    }

    if ident.token_literal() != "foobar" {
        panic!(
            "ident.token_literal() is not 'foobar'. got={}",
            ident.token_literal()
        );
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";

    let lexer = new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    check_parser_errors(&parser);

    if program.is_none() {
        panic!("parse_program() returned None");
    }
    let p = program.unwrap();

    if p.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statement. got={}",
            p.statements.len()
        );
    }

    let stmt = p.statements[0]
        .as_any()
        .downcast_ref::<ExpressionStatement>();
    let stmt = stmt.unwrap_or_else(|| {
        panic!("p.statements[0] is not ExpressionStatement.");
    });

    let literal = match stmt
        .expression
        .as_ref()
        .unwrap()
        .as_any()
        .downcast_ref::<IntegerLiteral>()
    {
        Some(literal) => literal,
        None => panic!("stmt.expression is not IntegerLiteral."),
    };

    if literal.value != 5 {
        panic!("literal.value is not 5. got={}", literal.value);
    }

    if literal.token_literal() != "5" {
        panic!(
            "literal.token_literal() is not {}. got={}",
            5,
            literal.token_literal()
        );
    }
}
