//! Evaluator for the Monkey programming language

use ast::ast::{Expression, Node, Statement};
use object::object::{Boolean, Integer, Null, ObjectTypes, INTEGER_OBJ};

const NULL: ObjectTypes = ObjectTypes::Null(Null {});
const TRUE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: true });
const FALSE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: false });

pub fn eval(node: &Node) -> Option<ObjectTypes> {
    let result = match node {
        Node::Program(program) => eval_statements(&program.statements),
        Node::Expression(expr) => match expr {
            Expression::IntegerLiteral(il) => {
                Some(ObjectTypes::Integer(Integer { value: il.value }))
            }
            Expression::Boolean(boolean) => {
                if boolean.value {
                    Some(TRUE)
                } else {
                    Some(FALSE)
                }
            }
            Expression::Prefix(prefix) => {
                let right = eval(prefix.right.as_ref().unwrap());
                return eval_prefix_expression(&prefix.operator, right);
            }
            _ => return None,
        },
        Node::Statement(stmt) => match stmt {
            Statement::ExpressionStatement(es) => eval(es.expression.as_ref().unwrap()),
            _ => return None,
        },
    };
    result
}

fn eval_statements(stmts: &[Node]) -> Option<ObjectTypes> {
    let mut result = None;

    for stmt in stmts {
        result = eval(stmt);
    }

    result
}

fn eval_prefix_expression(operator: &str, right: Option<ObjectTypes>) -> Option<ObjectTypes> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => None,
    }
}

fn eval_bang_operator_expression(right: Option<ObjectTypes>) -> Option<ObjectTypes> {
    match right {
        Some(ObjectTypes::Boolean(boolean)) => {
            if boolean.value {
                Some(FALSE)
            } else {
                Some(TRUE)
            }
        }
        Some(ObjectTypes::Null(_)) => Some(TRUE),
        _ => Some(FALSE),
    }
}

fn eval_minus_prefix_operator_expression(right: Option<ObjectTypes>) -> Option<ObjectTypes> {
    let right = match right {
        Some(obj) => obj,
        None => return None,
    };

    if right._type() != INTEGER_OBJ {
        return Some(ObjectTypes::Null(Null {}));
    }

    match right {
        ObjectTypes::Integer(integer) => Some(ObjectTypes::Integer(Integer {
            value: -integer.value,
        })),
        _ => None,
    }
}
