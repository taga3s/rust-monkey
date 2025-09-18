//! Evaluator for the Monkey programming language

use ast::ast::{BlockStatement, Expression, Identifier, IfExpression, Node, Program, Statement};
use object::{
    environment::Environment,
    object::{
        Boolean, Integer, Null, ObjectTypes, ReturnValue, ERROR_OBJ, INTEGER_OBJ, RETURN_VALUE_OBJ,
    },
};

const NULL: ObjectTypes = ObjectTypes::Null(Null {});
const TRUE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: true });
const FALSE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: false });

fn new_error(message: String) -> Option<ObjectTypes> {
    Some(ObjectTypes::Error(object::object::Error { message }))
}

fn is_error(obj: &Option<ObjectTypes>) -> bool {
    if let Some(obj) = obj {
        return obj._type() == ERROR_OBJ;
    }
    false
}

pub fn eval(node: &Node, env: &mut Environment) -> Option<ObjectTypes> {
    let result = match node {
        Node::Program(program) => eval_program(&program, env),
        Node::Expression(expr) => match expr {
            Expression::IntegerLiteral(il) => {
                Some(ObjectTypes::Integer(Integer { value: il.value }))
            }
            Expression::Boolean(boolean) => native_bool_to_boolean_object(boolean.value),
            Expression::Infix(infix) => {
                let left = eval(infix.left.as_ref().unwrap(), env);
                if is_error(&left) {
                    return left;
                }
                let right = eval(infix.right.as_ref().unwrap(), env);
                if is_error(&right) {
                    return right;
                }
                return eval_infix_expression(&infix.operator, left, right);
            }
            Expression::Prefix(prefix) => {
                let right = eval(prefix.right.as_ref().unwrap(), env);
                if is_error(&right) {
                    return right;
                }
                return eval_prefix_expression(&prefix.operator, right);
            }
            Expression::IfExpression(ifexp) => {
                return eval_if_expression(ifexp, env);
            }
            Expression::Identifier(ident) => eval_identifier(ident, env),
            _ => None,
        },
        Node::Statement(stmt) => match stmt {
            Statement::ExpressionStatement(es) => eval(es.expression.as_ref().unwrap(), env),
            Statement::Let(ls) => {
                let val = eval(ls.value.as_ref().unwrap(), env);
                if is_error(&val) {
                    return val;
                }
                env.set(ls.name.as_ref().unwrap().value.clone(), val.unwrap());
                None
            }
            Statement::Return(rs) => {
                let val = eval(rs.return_value.as_ref().unwrap(), env);
                if is_error(&val) {
                    return val;
                }
                return Some(ObjectTypes::ReturnValue(ReturnValue {
                    value: Box::new(val.unwrap()),
                }));
            }
            Statement::BlockStatement(bs) => eval_block_statement(bs, env),
        },
    };
    result
}

fn eval_program(program: &Program, env: &mut Environment) -> Option<ObjectTypes> {
    let mut result = None;

    for stmt in &program.statements {
        result = eval(&stmt, env);

        if let Some(r) = &result {
            match r {
                ObjectTypes::ReturnValue(return_value) => {
                    return Some(*return_value.value.clone());
                }
                ObjectTypes::Error(_) => return result,
                _ => {}
            }
        }
    }
    result
}

fn eval_block_statement(bs: &BlockStatement, env: &mut Environment) -> Option<ObjectTypes> {
    let mut result = None;

    for stmt in &bs.statements {
        result = eval(&stmt, env);

        if let Some(r) = &result {
            if r._type() == RETURN_VALUE_OBJ || r._type() == ERROR_OBJ {
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
        _ => new_error(format!(
            "unknown operator: {}{}",
            operator,
            right.unwrap()._type()
        )),
    }
}

fn eval_identifier(node: &Identifier, env: &mut Environment) -> Option<ObjectTypes> {
    match env.get(&node.value) {
        Some(val) => Some(val.clone()),
        None => new_error(format!("identifier not found: {}", node.value)),
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
        return new_error(format!("unknown operator: -{}", right._type()));
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
    if &left._type() != &right._type() {
        return new_error(format!(
            "type mismatch: {} {} {}",
            left._type(),
            operator,
            right._type()
        ));
    }

    new_error(format!(
        "unknown operator: {} {} {}",
        left._type(),
        operator,
        right._type()
    ))
}

fn eval_integer_infix_expression(
    operator: &str,
    left: ObjectTypes,
    right: ObjectTypes,
) -> Option<ObjectTypes> {
    let left_val = match &left {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return None,
    };
    let right_val = match &right {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return None,
    };

    match operator {
        "+" => Some(ObjectTypes::Integer(Integer {
            value: left_val + right_val,
        })),
        "-" => Some(ObjectTypes::Integer(Integer {
            value: left_val - right_val,
        })),
        "*" => Some(ObjectTypes::Integer(Integer {
            value: left_val * right_val,
        })),
        "/" => Some(ObjectTypes::Integer(Integer {
            value: left_val / right_val,
        })),
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        _ => new_error(format!(
            "unknown operator: {} {} {}",
            &left._type(),
            operator,
            &right._type()
        )),
    }
}

fn eval_if_expression(ie: &IfExpression, env: &mut Environment) -> Option<ObjectTypes> {
    let condition = eval(ie.condition.as_ref().unwrap(), env);
    if is_truthy(condition) {
        return eval(ie.consequence.as_ref().unwrap(), env);
    } else if let Some(alt) = &ie.alternative {
        return eval(alt, env);
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
