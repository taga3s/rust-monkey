use ast::ast::{
    Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement,
    Node, PrefixExpression, ReturnStatement, Statement,
};
use lexer::lexer::Lexer;

use crate::parser::Parser;

// -- Test Helpers -- //
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

fn test_boolean_literal(exp: &Box<dyn Expression>, value: bool) -> bool {
    let boolean = match exp.as_any().downcast_ref::<ast::ast::Boolean>() {
        Some(boolean) => boolean,
        None => {
            panic!("exp is not Boolean.");
        }
    };

    if boolean.value != value {
        panic!("boolean.value is not {}. got={}", value, boolean.value);
    }

    if boolean.token_literal() != value.to_string() {
        panic!(
            "boolean.token_literal() is not {}. got={}",
            value,
            boolean.token_literal()
        );
    }

    true
}

fn test_identifier(exp: &Box<dyn Expression>, value: &str) -> bool {
    let ident = match exp.as_any().downcast_ref::<Identifier>() {
        Some(ident) => ident,
        None => {
            panic!("exp is not Identifier.");
        }
    };

    if ident.value != value {
        panic!("ident.value is not {}. got={}", value, ident.value);
    }

    if ident.token_literal() != value {
        panic!(
            "ident.token_literal() is not {}. got={}",
            value,
            ident.token_literal()
        );
    }

    true
}

fn test_literal_expression(exp: &Box<dyn Expression>, expected: TestingLiteral) -> bool {
    match expected {
        TestingLiteral::Int(value) => return test_integer_literal(exp, value),
        TestingLiteral::Str(value) => return test_identifier(exp, value),
        TestingLiteral::Bool(value) => return test_boolean_literal(exp, value),
        // _ => {
        //     eprintln!("type of exp not handled.");
        //     return false;
        // }
    }
}

fn test_infix_expression(
    exp: &Box<dyn Expression>,
    left: TestingLiteral,
    operator: &str,
    right: TestingLiteral,
) -> bool {
    let op_exp = match exp.as_any().downcast_ref::<InfixExpression>() {
        Some(op_exp) => op_exp,
        None => {
            panic!("exp is not InfixExpression.");
        }
    };

    if !test_literal_expression(&op_exp.left.as_ref().unwrap(), left) {
        return false;
    }

    if op_exp.operator != operator {
        panic!("operator is not {}. got={}", operator, op_exp.operator);
    }

    if !test_literal_expression(&op_exp.right.as_ref().unwrap(), right) {
        return false;
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

    let program = parser.parse_program();
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

    let program = parser.parse_program();
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

    let program = parser.parse_program();
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

    let program = parser.parse_program();
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
fn test_boolean_expression() {
    let tests = vec![("true;", true), ("false;", false)];

    for test in tests {
        let (input, value) = test;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
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

        let boolean = match stmt
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<ast::ast::Boolean>()
        {
            Some(boolean) => boolean,
            None => panic!("stmt.expression is not Boolean."),
        };

        if boolean.value != value {
            panic!("boolean.value is not {}. got={}", value, boolean.value);
        }

        if boolean.token_literal() != value.to_string() {
            panic!(
                "boolean.token_literal() is not {}. got={}",
                value.to_string(),
                boolean.token_literal()
            );
        };
    }
}

enum TestingLiteral {
    Int(i64),
    Str(&'static str),
    Bool(bool),
}
#[test]
fn test_parsing_prefix_expressions() {
    let prefix_tests = vec![
        ("!5;", "!", TestingLiteral::Int(5)),
        ("-15;", "-", TestingLiteral::Int(15)),
        ("!true;", "!", TestingLiteral::Bool(true)),
        ("!false;", "!", TestingLiteral::Bool(false)),
    ];

    for test in prefix_tests {
        let (input, operator, value) = test;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
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

        if !test_literal_expression(&expression.right.as_ref().unwrap(), value) {
            return;
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let infix_tests: Vec<(&'static str, TestingLiteral, &'static str, TestingLiteral)> = vec![
        (
            "5 + 5;",
            TestingLiteral::Int(5),
            "+",
            TestingLiteral::Int(5),
        ),
        (
            "5 - 5;",
            TestingLiteral::Int(5),
            "-",
            TestingLiteral::Int(5),
        ),
        (
            "5 * 5;",
            TestingLiteral::Int(5),
            "*",
            TestingLiteral::Int(5),
        ),
        (
            "5 / 5;",
            TestingLiteral::Int(5),
            "/",
            TestingLiteral::Int(5),
        ),
        (
            "5 > 5;",
            TestingLiteral::Int(5),
            ">",
            TestingLiteral::Int(5),
        ),
        (
            "5 < 5;",
            TestingLiteral::Int(5),
            "<",
            TestingLiteral::Int(5),
        ),
        (
            "5 == 5;",
            TestingLiteral::Int(5),
            "==",
            TestingLiteral::Int(5),
        ),
        (
            "5 != 5;",
            TestingLiteral::Int(5),
            "!=",
            TestingLiteral::Int(5),
        ),
        (
            "true == true",
            TestingLiteral::Bool(true),
            "==",
            TestingLiteral::Bool(true),
        ),
        (
            "true != false",
            TestingLiteral::Bool(true),
            "!=",
            TestingLiteral::Bool(false),
        ),
        (
            "false == false",
            TestingLiteral::Bool(false),
            "==",
            TestingLiteral::Bool(false),
        ),
    ];

    for test in infix_tests {
        let (input, left_value, operator, right_value) = test;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
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

        if !test_infix_expression(
            stmt.expression.as_ref().unwrap(),
            left_value,
            operator,
            right_value,
        ) {
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
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
    ];

    for test in tests {
        let (input, expected) = test;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        let actual = program.to_string();
        if actual != expected {
            panic!("expected={}, got={}", expected, actual);
        }
    }
}
