//! Evaluator for the Monkey programming language

use ast::ast::{BlockStatement, Expression, IfExpression, Node, Program, Statement};
use object::object::{
    Boolean, Integer, Null, ObjectTypes, ReturnValue, INTEGER_OBJ, RETURN_VALUE_OBJ,
};

const NULL: ObjectTypes = ObjectTypes::Null(Null {});
const TRUE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: true });
const FALSE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: false });

pub fn eval(node: &Node) -> Option<ObjectTypes> {
    let result = match node {
        Node::Program(program) => eval_program(&program),
        Node::Expression(expr) => match expr {
            Expression::IntegerLiteral(il) => {
                Some(ObjectTypes::Integer(Integer { value: il.value }))
            }
            Expression::Boolean(boolean) => native_bool_to_boolean_object(boolean.value),
            Expression::Infix(infix) => {
                let left = eval(infix.left.as_ref().unwrap());
                let right = eval(infix.right.as_ref().unwrap());
                return eval_infix_expression(&infix.operator, left, right);
            }
            Expression::Prefix(prefix) => {
                let right = eval(prefix.right.as_ref().unwrap());
                return eval_prefix_expression(&prefix.operator, right);
            }
            Expression::IfExpression(ifexp) => {
                return eval_if_expression(ifexp);
            }
            _ => return None,
        },
        Node::Statement(stmt) => match stmt {
            Statement::ExpressionStatement(es) => eval(es.expression.as_ref().unwrap()),
            Statement::Return(rs) => {
                let val = eval(rs.return_value.as_ref().unwrap());
                return Some(ObjectTypes::ReturnValue(ReturnValue {
                    value: Box::new(val.unwrap()),
                }));
            }
            Statement::BlockStatement(bs) => eval_block_statement(&bs),
            _ => return None,
        },
    };
    result
}

fn eval_program(program: &Program) -> Option<ObjectTypes> {
    let mut result = None;

    for stmt in &program.statements {
        result = eval(&stmt);

        if let Some(ObjectTypes::ReturnValue(return_value)) = result {
            return Some(*return_value.value);
        }
    }
    result
}

fn eval_block_statement(bs: &BlockStatement) -> Option<ObjectTypes> {
    let mut result = None;

    for stmt in &bs.statements {
        result = eval(&stmt);

        if let Some(r) = &result {
            if r._type() == RETURN_VALUE_OBJ {
                return result;
            }
        }
    }
    result
}

fn native_bool_to_boolean_object(input: bool) -> Option<ObjectTypes> {
    if input {
        Some(TRUE)
    } else {
        Some(FALSE)
    }
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

fn eval_infix_expression(
    operator: &str,
    left: Option<ObjectTypes>,
    right: Option<ObjectTypes>,
) -> Option<ObjectTypes> {
    let left = match left {
        Some(obj) => obj,
        None => return None,
    };
    let right = match right {
        Some(obj) => obj,
        None => return None,
    };

    if &left._type() == INTEGER_OBJ && &right._type() == INTEGER_OBJ {
        return eval_integer_infix_expression(operator, left, right);
    };
    if operator == "==" {
        return native_bool_to_boolean_object(left == right);
    }
    if operator == "!=" {
        return native_bool_to_boolean_object(left.inspect() != right.inspect());
    }

    None
}

fn eval_integer_infix_expression(
    operator: &str,
    left: ObjectTypes,
    right: ObjectTypes,
) -> Option<ObjectTypes> {
    let left = match left {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return None,
    };
    let right = match right {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return None,
    };

    match operator {
        "+" => Some(ObjectTypes::Integer(Integer {
            value: left + right,
        })),
        "-" => Some(ObjectTypes::Integer(Integer {
            value: left - right,
        })),
        "*" => Some(ObjectTypes::Integer(Integer {
            value: left * right,
        })),
        "/" => Some(ObjectTypes::Integer(Integer {
            value: left / right,
        })),
        "<" => native_bool_to_boolean_object(left < right),
        ">" => native_bool_to_boolean_object(left > right),
        "==" => native_bool_to_boolean_object(left == right),
        "!=" => native_bool_to_boolean_object(left != right),
        _ => None,
    }
}

fn eval_if_expression(ie: &IfExpression) -> Option<ObjectTypes> {
    let condition = eval(ie.condition.as_ref().unwrap());
    if is_truthy(condition) {
        return eval(ie.consequence.as_ref().unwrap());
    } else if let Some(alt) = &ie.alternative {
        return eval(alt);
    } else {
        return Some(NULL);
    }
}

fn is_truthy(obj: Option<ObjectTypes>) -> bool {
    match obj {
        Some(ObjectTypes::Boolean(boolean)) => boolean.value,
        Some(ObjectTypes::Null(_)) => false,
        _ => true,
    }
}
