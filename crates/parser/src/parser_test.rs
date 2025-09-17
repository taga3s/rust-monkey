use ast::ast::{Expression, Node, Statement, TNode};
use lexer::lexer::Lexer;

use crate::parser::Parser;

// -- Test Helpers -- //
#[derive(Clone)]
enum TestingLiteral {
    Int(i64),
    Str(&'static str),
    Bool(bool),
}

fn test_literal_expression(exp: &Expression, expected: TestingLiteral) -> bool {
    match expected {
        TestingLiteral::Int(value) => return test_integer_literal(exp, value),
        TestingLiteral::Str(value) => return test_identifier(exp, value),
        TestingLiteral::Bool(value) => return test_boolean_literal(exp, value),
    }
}

fn test_integer_literal(il: &Expression, value: i64) -> bool {
    let integer = match il {
        Expression::IntegerLiteral(integer) => integer,
        _ => {
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

fn test_identifier(ident: &Expression, value: &str) -> bool {
    let ident = match ident {
        Expression::Identifier(ident) => ident,
        _ => {
            panic!("ident is not Identifier.");
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

fn test_boolean_literal(exp: &Expression, value: bool) -> bool {
    let boolean = match exp {
        Expression::Boolean(boolean) => boolean,
        _ => {
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

fn test_infix_expression(
    exp: &Expression,
    left: TestingLiteral,
    operator: &str,
    right: TestingLiteral,
) -> bool {
    let op_exp = match exp {
        Expression::Infix(op_exp) => op_exp,
        _ => {
            panic!("exp is not InfixExpression.");
        }
    };

    let left_exp = match op_exp.left.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("op_exp.left is not Expression.");
        }
    };

    if !test_literal_expression(left_exp, left) {
        return false;
    }

    if op_exp.operator != operator {
        panic!("operator is not {}. got={}", operator, op_exp.operator);
    }

    let right_exp = match op_exp.right.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("op_exp.right is not Expression.");
        }
    };

    if !test_literal_expression(right_exp, right) {
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
    let tests = vec![
        ("let x = 5;", "x", TestingLiteral::Int(5)),
        ("let y = true;", "y", TestingLiteral::Bool(true)),
        ("let foobar = y;", "foobar", TestingLiteral::Str("y")),
    ];

    for test in tests {
        let (input, ident, expected) = test;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Node::Program(p) => p,
            _ => {
                panic!("parser.parse_program() did not return Program.");
            }
        };
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement. got={}",
                program.statements.len()
            );
        }

        let stmt = match &program.statements[0] {
            Node::Statement(s) => s,
            _ => {
                panic!("program.statements[0] is not Statement.");
            }
        };
        if !test_let_statement(stmt, ident) {
            return;
        }

        let let_stmt_value = match stmt {
            Statement::Let(s) => match s.value.as_ref().unwrap().as_ref() {
                Node::Expression(e) => e,
                _ => {
                    panic!("let_stmt.value is not Expression.");
                }
            },
            _ => {
                panic!("stmt is not LetStatement.");
            }
        };
        if !test_literal_expression(let_stmt_value, expected) {
            return;
        }
    }

    fn test_let_statement(stmt: &Statement, ident: &str) -> bool {
        if !matches!(stmt, Statement::Let(_)) {
            panic!("stmt is not LetStatement.");
        }

        let let_stmt = match stmt {
            Statement::Let(s) => s,
            _ => {
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
        Node::Program(p) => p,
        _ => {
            panic!("parser.parse_program() did not return Program.");
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
        let return_stmt = match stmt {
            Node::Statement(Statement::Return(s)) => s,
            _ => {
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
        Node::Program(p) => p,
        _ => {
            panic!("parser.parse_program() did not return Program.");
        }
    };
    check_parser_errors(&parser);

    if program.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );
    }

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let ident = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::Identifier(ident)) => ident,
        _ => panic!("stmt.expression is not Identifier."),
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
        Node::Program(p) => p,
        _ => {
            panic!("parser.parse_program() did not return Program.");
        }
    };
    check_parser_errors(&parser);

    if program.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );
    }

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let literal = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::IntegerLiteral(literal)) => literal,
        _ => panic!("stmt.expression is not IntegerLiteral."),
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

        let program = match parser.parse_program() {
            Node::Program(p) => p,
            _ => {
                panic!("parser.parse_program() did not return Program.");
            }
        };
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement. got={}",
                program.statements.len()
            );
        }

        let stmt = match &program.statements[0] {
            Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
            _ => panic!("program.statements[0] is not ExpressionStatement."),
        };

        let boolean = match stmt.expression.as_ref().unwrap().as_ref() {
            Node::Expression(Expression::Boolean(boolean)) => boolean,
            _ => panic!("stmt.expression is not Boolean."),
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

        let program = match parser.parse_program() {
            Node::Program(p) => p,
            _ => {
                panic!("parser.parse_program() did not return Program.");
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

        let stmt = match &program.statements[0] {
            Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
            _ => panic!("program.statements[0] is not ExpressionStatement."),
        };

        let expression = match stmt.expression.as_ref().unwrap().as_ref() {
            Node::Expression(Expression::Prefix(expression)) => expression,
            _ => panic!("stmt.expression is not PrefixExpression."),
        };

        if expression.operator != operator {
            panic!(
                "expression.operator is not '{}'. got={}",
                operator, expression.operator
            );
        }

        let right_exp = match expression.right.as_ref().unwrap().as_ref() {
            Node::Expression(e) => e,
            _ => {
                panic!("expression.right is not Expression.");
            }
        };

        if !test_literal_expression(right_exp, value) {
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

        let program = match parser.parse_program() {
            Node::Program(p) => p,
            _ => {
                panic!("parser.parse_program() did not return Program.");
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

        let stmt = match &program.statements[0] {
            Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
            _ => panic!("program.statements[0] is not ExpressionStatement."),
        };

        let exp = match stmt.expression.as_ref().unwrap().as_ref() {
            Node::Expression(exp) => exp,
            _ => panic!("stmt.expression is not InfixExpression."),
        };

        if !test_infix_expression(exp, left_value, operator, right_value) {
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
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
    ];

    for test in tests {
        let (input, expected) = test;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Node::Program(p) => p,
            _ => {
                panic!("parser.parse_program() did not return Program.");
            }
        };
        check_parser_errors(&parser);

        let actual = program.to_string();
        if actual != expected {
            panic!("expected={}, got={}", expected, actual);
        }
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = match parser.parse_program() {
        Node::Program(p) => p,
        _ => {
            panic!("parser.parse_program() did not return Program.");
        }
    };
    check_parser_errors(&parser);

    if program.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );
    }

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let ifexp = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::IfExpression(expression)) => expression,
        _ => panic!("stmt.expression is not IfExpression."),
    };

    let exp = match ifexp.condition.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("ifexp.condition is not Expression.");
        }
    };

    if !test_infix_expression(exp, TestingLiteral::Str("x"), "<", TestingLiteral::Str("y")) {
        return;
    }

    if ifexp.consequence.is_none() {
        panic!("ifexp.consequence is None.");
    }

    let if_exp_consequence = match ifexp.consequence.as_ref().unwrap().as_ref() {
        Node::Statement(Statement::BlockStatement(b)) => b,
        _ => {
            panic!("ifexp.consequence is not BlockStatement.");
        }
    };

    if if_exp_consequence.statements.len() != 1 {
        panic!(
            "if_exp_consequence.statements does not contain 1 statement. got={}",
            if_exp_consequence.statements.len()
        );
    }

    let consequence = match &if_exp_consequence.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("ifexp.consequence.statements[0] is not ExpressionStatement."),
    };

    let ident = match consequence.expression.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => panic!("consequence.expression is not Identifier."),
    };
    if !test_identifier(ident, "x") {
        return;
    }

    if ifexp.alternative.is_some() {
        panic!("ifexp.alternative is not None.");
    }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = match parser.parse_program() {
        Node::Program(p) => p,
        _ => {
            panic!("parser.parse_program() did not return Program.");
        }
    };
    check_parser_errors(&parser);

    if program.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );
    }

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let ifexp = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::IfExpression(expression)) => expression,
        _ => panic!("stmt.expression is not IfExpression."),
    };

    let condition = match ifexp.condition.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("ifexp.condition is not Expression.");
        }
    };

    if !test_infix_expression(
        condition,
        TestingLiteral::Str("x"),
        "<",
        TestingLiteral::Str("y"),
    ) {
        return;
    }

    if ifexp.consequence.is_none() {
        panic!("ifexp.consequence is None.");
    }

    let if_exp_consequence = match ifexp.consequence.as_ref().unwrap().as_ref() {
        Node::Statement(Statement::BlockStatement(b)) => b,
        _ => {
            panic!("ifexp.consequence is not BlockStatement.");
        }
    };

    if if_exp_consequence.statements.len() != 1 {
        panic!(
            "if_exp_consequence.statements does not contain 1 statement. got={}",
            if_exp_consequence.statements.len()
        );
    }

    let consequence = match &if_exp_consequence.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("ifexp.consequence.statements[0] is not ExpressionStatement."),
    };

    let ident = match consequence.expression.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => panic!("consequence.expression is not Identifier."),
    };

    if !test_identifier(ident, "x") {
        return;
    }

    if ifexp.alternative.is_none() {
        panic!("ifexp.alternative is None.");
    }

    let if_exp_alternative = match ifexp.alternative.as_ref().unwrap().as_ref() {
        Node::Statement(Statement::BlockStatement(b)) => b,
        _ => {
            panic!("ifexp.alternative is not BlockStatement.");
        }
    };

    if if_exp_alternative.statements.len() != 1 {
        panic!(
            "if_exp_alternative.statements does not contain 1 statement. got={}",
            if_exp_alternative.statements.len()
        );
    }

    let alternative = match &if_exp_alternative.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("ifexp.alternative.statements[0] is not ExpressionStatement."),
    };

    let ident = match alternative.expression.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => panic!("alternative.expression is not Identifier."),
    };

    if !test_identifier(ident, "y") {
        return;
    }
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = match parser.parse_program() {
        Node::Program(p) => p,
        _ => {
            panic!("parser.parse_program() did not return Program.");
        }
    };
    check_parser_errors(&parser);

    if program.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );
    }

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let function = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::FunctionLiteral(function)) => function,
        _ => panic!("stmt.expression is not FunctionLiteral."),
    };

    if function.parameters.len() != 2 {
        panic!(
            "function.parameters does not contain 2 parameters. got={}",
            function.parameters.len()
        );
    }

    let param0 = match function.parameters[0].as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("function.parameters[0] is not Expression.");
        }
    };

    if !test_literal_expression(param0, TestingLiteral::Str("x")) {
        return;
    }

    let param1 = match function.parameters[1].as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("function.parameters[1] is not Expression.");
        }
    };

    if !test_literal_expression(param1, TestingLiteral::Str("y")) {
        return;
    }

    let function_body = match function.body.as_ref().unwrap().as_ref() {
        Node::Statement(Statement::BlockStatement(b)) => b,
        _ => {
            panic!("function.body is not BlockStatement.");
        }
    };

    if function_body.statements.len() != 1 {
        panic!(
            "function.body.statements does not contain 1 statement. got={}",
            function_body.statements.len()
        );
    }

    let body_stmt = match &function_body.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("function.body.statements[0] is not ExpressionStatement."),
    };

    let exp = match body_stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => panic!("body_stmt.expression is not InfixExpression."),
    };

    if !test_infix_expression(exp, TestingLiteral::Str("x"), "+", TestingLiteral::Str("y")) {
        return;
    }
}

#[test]
fn test_function_parameter_parsing() {
    let tests = vec![
        ("fn() {};", vec![]),
        ("fn(x) {};", vec![TestingLiteral::Str("x")]),
        (
            "fn(x, y, z) {};",
            vec![
                TestingLiteral::Str("x"),
                TestingLiteral::Str("y"),
                TestingLiteral::Str("z"),
            ],
        ),
    ];

    for test in tests {
        let (input, expected_params) = test;

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Node::Program(p) => p,
            _ => {
                panic!("parser.parse_program() did not return Program.");
            }
        };
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statement. got={}",
                program.statements.len()
            );
        }

        let stmt = match &program.statements[0] {
            Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
            _ => panic!("program.statements[0] is not ExpressionStatement."),
        };

        let function = match stmt.expression.as_ref().unwrap().as_ref() {
            Node::Expression(Expression::FunctionLiteral(function)) => function,
            _ => panic!("stmt.expression is not FunctionLiteral."),
        };

        if function.parameters.len() != expected_params.len() {
            panic!(
                "function.parameters does not contain {} parameters. got={}",
                expected_params.len(),
                function.parameters.len()
            );
        }

        for (i, ident) in expected_params.iter().enumerate() {
            let param = match function.parameters[i].as_ref() {
                Node::Expression(e) => e,
                _ => {
                    panic!("function.parameters[{}] is not Expression.", i);
                }
            };
            if !test_literal_expression(param, ident.clone()) {
                return;
            }
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);

    let program = match parser.parse_program() {
        Node::Program(p) => p,
        _ => {
            panic!("parser.parse_program() did not return Program.");
        }
    };
    check_parser_errors(&parser);

    if program.statements.len() != 1 {
        panic!(
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );
    }

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let exp = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::CallExpression(exp)) => exp,
        _ => panic!("stmt.expression is not CallExpression."),
    };

    let function = match exp.function.as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("exp.function is not Expression.");
        }
    };

    if !test_identifier(function, "add") {
        return;
    }

    if exp.arguments.len() != 3 {
        panic!(
            "exp.arguments does not contain 3 arguments. got={}",
            exp.arguments.len()
        );
    }

    let exp0 = match &exp.arguments[0].as_ref() {
        Node::Expression(exps) => exps,
        _ => {
            panic!("exp.arguments is not Expressions.");
        }
    };

    if !test_literal_expression(exp0, TestingLiteral::Int(1)) {
        return;
    }

    let exp1 = match &exp.arguments[1].as_ref() {
        Node::Expression(exps) => exps,
        _ => {
            panic!("exp.arguments is not Expressions.");
        }
    };

    if !test_infix_expression(exp1, TestingLiteral::Int(2), "*", TestingLiteral::Int(3)) {
        return;
    }

    let exp2 = match &exp.arguments[2].as_ref() {
        Node::Expression(exps) => exps,
        _ => {
            panic!("exp.arguments is not Expressions.");
        }
    };

    if !test_infix_expression(exp2, TestingLiteral::Int(4), "+", TestingLiteral::Int(5)) {
        return;
    }
}
