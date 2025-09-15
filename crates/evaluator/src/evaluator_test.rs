use lexer::lexer::Lexer;
use object::object::ObjectTypes;
use parser::parser::Parser;

use crate::evaluator::eval;

fn test_eval(input: &str) -> ObjectTypes {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    eval(&program).unwrap()
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
    let tests = vec![("5", 5), ("10", 10)];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = vec![("true", true), ("false", false)];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, expected);
    }
}
