use ast::ast::TNode;
use lexer::lexer::Lexer;
use object::{
    environment::new_environment,
    object::{Null, ObjectTypes},
};
use parser::parser::Parser;

use crate::evaluator::eval;

// -- Test Helpers -- //
#[derive(Clone)]
enum TestingLiteral {
    Int(i64),
    Str(&'static str),
    Bool(bool),
}

fn test_eval(input: &str) -> ObjectTypes {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    let mut env = new_environment();

    eval(&program, &mut env)
}

fn test_integer_object(obj: ObjectTypes, expected: i64) {
    let value = match obj {
        ObjectTypes::Integer(integer) => integer.value,
        _ => panic!("object is not Integer. got={}", obj.inspect()),
    };

    assert_eq!(value, expected);

    true;
}

fn test_boolean_object(obj: ObjectTypes, expected: bool) {
    let value = match obj {
        ObjectTypes::Boolean(boolean) => boolean.value,
        _ => panic!("object is not Boolean. got={}", obj.inspect()),
    };

    assert_eq!(value, expected);

    true;
}

#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 /3) * 2 + -10", 50),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_string_literal() {
    let input = r#""Hello, World!""#;

    let evaluated = test_eval(input);
    match evaluated {
        ObjectTypes::StringLiteral(string) => {
            assert_eq!(string.value, "Hello, World!");
        }
        _ => panic!(
            "evaluated is not StringLiteral. got={}",
            evaluated.inspect()
        ),
    }
}

#[test]
fn test_string_concatenation() {
    let input = r#""Hello, " + "World!""#;

    let evaluated = test_eval(input);
    match evaluated {
        ObjectTypes::StringLiteral(string) => {
            assert_eq!(string.value, "Hello, World!");
        }
        _ => panic!(
            "evaluated is not StringLiteral. got={}",
            evaluated.inspect()
        ),
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, expected);
    }
}

#[test]
fn test_bang_operator() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, expected);
    }
}

#[test]
fn test_if_else_expressions() {
    let tests = vec![
        ("if (true) { 10 }", Some(10)),
        ("if (false) { 10 }", None),
        ("if (1) { 10 }", Some(10)),
        ("if (1 < 2) { 10 }", Some(10)),
        ("if (1 > 2) { 10 }", None),
        ("if (1 > 2) { 10 } else { 20 }", Some(20)),
        ("if (1 < 2) { 10 } else { 20 }", Some(10)),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);
        match test.1 {
            Some(expected) => {
                test_integer_object(evaluated, expected);
            }
            None => {
                test_null_object(evaluated);
            }
        }
    }
}

fn test_null_object(obj: ObjectTypes) -> bool {
    match obj {
        ObjectTypes::Null(Null {}) => true,
        _ => panic!("object is not Null. got={}", obj.inspect()),
    }
}

#[test]
fn test_return_statements() {
    let tests = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_error_handling() {
    let tests = vec![
        ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
        ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
        (
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        (
            "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        ("foobar", "identifier not found: foobar"),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);

        match evaluated {
            ObjectTypes::Error(error) => {
                assert_eq!(error.message, test.1);
            }
            _ => {
                eprintln!("object is not Error. got={}", evaluated.inspect());
            }
        }
    }
}

#[test]
fn test_let_statements() {
    let tests = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";

    let evaluated = test_eval(input);
    match evaluated {
        ObjectTypes::Function(function) => {
            assert_eq!(function.parameters.len(), 1);
            assert_eq!(function.parameters[0].to_string(), "x");
            assert_eq!(function.body.to_string(), "(x + 2)");
        }
        _ => {
            panic!("object is not Function. got={}", evaluated.inspect());
        }
    }
}

#[test]
fn test_function_application() {
    let tests = vec![
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_closures() {
    let input = "
    let newAdder = fn(x) {
        fn(y) { x + y };
    };
    let addTwo = newAdder(2);
    addTwo(3);
    ";

    let evaluated = test_eval(input);
    test_integer_object(evaluated, 5);
}

#[test]
fn test_builtin_functions() {
    let tests = vec![
        (r#"len("")"#, TestingLiteral::Int(0)),
        (r#"len("four")"#, TestingLiteral::Int(4)),
        (r#"len("hello world")"#, TestingLiteral::Int(11)),
        (
            "len(1)",
            TestingLiteral::Str("argument to `len` not supported, got INTEGER"),
        ),
        (
            r#"len("one", "two")"#,
            TestingLiteral::Str("wrong number of arguments. got=2, want=1"),
        ),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);

        match expected {
            TestingLiteral::Int(expected) => {
                test_integer_object(evaluated, expected);
            }
            TestingLiteral::Str(expected) => match evaluated {
                ObjectTypes::Error(error) => {
                    assert_eq!(error.message, expected);
                }
                _ => {
                    panic!("object is not Error. got={}", evaluated.inspect());
                }
            },
            _ => panic!("test case error"),
        }
    }
}
