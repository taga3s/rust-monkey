use ast::ast::{
    Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement,
    Node, PrefixExpression, ReturnStatement, Statement,
};
use lexer::lexer::Lexer;

use crate::parser::Parser;

fn test_integer_literal(il: &Box<dyn Expression>, value: i64) -> bool {
    let integer = match il.as_any().downcast_ref::<IntegerLiteral>() {
        Some(integer) => integer,
        None => {
            panic!("il is not IntegerLiteral.");
        }
    };

    if integer.value != value {
        panic!("integer.value is not {}. got={}", value, integer.value);
    }

    if integer.token_literal() != value.to_string() {
        panic!(
            "integer.token_literal() is not {}. got={}",
            value,
            integer.token_literal()
        );
    }

    true
}

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

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = match parser.parse_program() {
        Some(p) => p,
        None => {
            panic!("parser.parse_program() returned None");
        }
    };
    check_parser_errors(&parser);

    if program.statements.len() != 3 {
        panic!(
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );
    }

    let tests = vec!["x", "y", "foobar"];

    for (i, ident) in tests.iter().enumerate() {
        let stmt = &program.statements[i];
        if !test_let_statement(stmt, ident) {
            return;
        }
    }

    fn test_let_statement(stmt: &Box<dyn Statement>, ident: &str) -> bool {
        if stmt.token_literal() != "let" {
            panic!("stmt.token_literal not 'let'. got={}", stmt.token_literal());
        }

        let let_stmt = match stmt.as_any().downcast_ref::<LetStatement>() {
            Some(let_stmt) => let_stmt,
            None => {
                panic!("stmt is not LetStatement.");
            }
        };

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

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = match parser.parse_program() {
        Some(p) => p,
        None => {
            panic!("parser.parse_program() returned None");
        }
    };
    check_parser_errors(&parser);

    if program.statements.len() != 3 {
        panic!(
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );
    }

    for stmt in program.statements.iter() {
        let return_stmt = match stmt.as_any().downcast_ref::<ReturnStatement>() {
            Some(return_stmt) => return_stmt,
            None => {
                panic!("stmt is not ReturnStatement.");
            }
        };

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

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = match parser.parse_program() {
        Some(p) => p,
        None => {
            panic!("parser.parse_program() returned None");
        }
    };
    check_parser_errors(&parser);

    if program.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );
    }

    let stmt = match program.statements[0]
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

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = match parser.parse_program() {
        Some(p) => p,
        None => {
            panic!("parser.parse_program() returned None");
        }
    };
    check_parser_errors(&parser);

    if program.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );
    }

    let stmt = match program.statements[0]
        .as_any()
        .downcast_ref::<ExpressionStatement>()
    {
        Some(stmt) => stmt,
        None => panic!("program.statements[0] is not ExpressionStatement."),
    };

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

#[test]
fn test_parsing_prefix_expressions() {
    let prefix_tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];

    for test in prefix_tests {
        let (input, operator, integer_value) = test;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Some(p) => p,
            None => {
                panic!("parser.parse_program() returned None");
            }
        };
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statement. got={}",
                1,
                program.statements.len()
            );
        }

        let stmt = match program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
        {
            Some(stmt) => stmt,
            None => panic!("program.statements[0] is not ExpressionStatement."),
        };

        let expression = match stmt
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<PrefixExpression>()
        {
            Some(expression) => expression,
            None => panic!("stmt.expression is not PrefixExpression."),
        };

        if expression.operator != operator {
            panic!(
                "expression.operator is not '{}'. got={}",
                operator, expression.operator
            );
        }

        if !test_integer_literal(&expression.right.as_ref().unwrap(), integer_value) {
            return;
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let infix_tests = vec![
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5),
    ];

    for test in infix_tests {
        let (input, left_value, operator, right_value) = test;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Some(p) => p,
            None => {
                panic!("parser.parse_program() returned None");
            }
        };
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statement. got={}",
                1,
                program.statements.len()
            );
        }

        let stmt = match program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
        {
            Some(stmt) => stmt,
            None => panic!("program.statements[0] is not ExpressionStatement."),
        };

        let expression = match stmt
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<InfixExpression>()
        {
            Some(expression) => expression,
            None => panic!("stmt.expression is not InfixExpression."),
        };

        if !test_integer_literal(&expression.left.as_ref().unwrap(), left_value) {
            return;
        }

        if expression.operator != operator {
            panic!(
                "expression.operator is not '{}'. got={}",
                operator, expression.operator
            );
        }

        if !test_integer_literal(&expression.right.as_ref().unwrap(), right_value) {
            return;
        }
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
    ];

    for test in tests {
        let (input, expected) = test;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Some(p) => p,
            None => {
                panic!("parser.parse_program() returned None");
            }
        };
        check_parser_errors(&parser);

        let actual = program.to_string();
        if actual != expected {
            panic!("expected={}, got={}", expected, actual);
        }
    }
}
