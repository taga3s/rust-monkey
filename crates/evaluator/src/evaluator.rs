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

pub fn eval(node: &Node, env: &mut Environment) -> ObjectTypes {
    let result = match node {
        Node::Program(program) => eval_program(&program, env),
        Node::Expression(expr) => match expr {
            Expression::IntegerLiteral(il) => ObjectTypes::Integer(Integer { value: il.value }),
            Expression::Boolean(boolean) => native_bool_to_boolean_object(boolean.value),
            Expression::Identifier(ident) => eval_identifier(ident, env),
            Expression::Infix(infix) => {
                let left = eval(infix.left.as_ref().unwrap(), env);
                if is_error(&left) {
                    return left;
                }
                let right = eval(infix.right.as_ref().unwrap(), env);
                if is_error(&right) {
                    return right;
                }
                return eval_infix_expression(&infix.operator, &left, &right);
            }
            Expression::Prefix(prefix) => {
                let right = eval(prefix.right.as_ref().unwrap(), env);
                if is_error(&right) {
                    return right;
                }
                return eval_prefix_expression(&prefix.operator, &right);
            }
            Expression::IfExpression(ifexp) => eval_if_expression(ifexp, env),
            _ => NULL, //FIXME: handle other object types
        },
        Node::Statement(stmt) => match stmt {
            Statement::ExpressionStatement(es) => eval(es.expression.as_ref().unwrap(), env),
            Statement::Let(ls) => {
                let val = eval(ls.value.as_ref().unwrap(), env);
                if is_error(&val) {
                    return val;
                }
                env.set(ls.name.as_ref().unwrap().value.clone(), val);
                NULL //FIXME: handle other object types
            }
            Statement::Return(rs) => {
                let val = eval(rs.return_value.as_ref().unwrap(), env);
                if is_error(&val) {
                    return val;
                }
                return ObjectTypes::ReturnValue(ReturnValue {
                    value: Box::new(val),
                });
            }
            Statement::BlockStatement(bs) => eval_block_statement(bs, env),
        },
    };
    result
}

fn eval_program(program: &Program, env: &mut Environment) -> ObjectTypes {
    let mut result = ObjectTypes::Null(Null {}); //FIXME: handle other object types

    for stmt in &program.statements {
        result = eval(&stmt, env);

        match result {
            ObjectTypes::ReturnValue(return_value) => {
                return *return_value.value.clone();
            }
            ObjectTypes::Error(_) => return result,
            _ => {}
        }
    }
    result
}

fn eval_block_statement(bs: &BlockStatement, env: &mut Environment) -> ObjectTypes {
    let mut result = ObjectTypes::Null(Null {});

    for stmt in &bs.statements {
        result = eval(&stmt, env);

        if result._type() == RETURN_VALUE_OBJ || result._type() == ERROR_OBJ {
            return result;
        }
    }
    result
}

fn new_error(message: String) -> ObjectTypes {
    ObjectTypes::Error(object::object::Error { message })
}

fn is_error(obj: &ObjectTypes) -> bool {
    obj._type() == ERROR_OBJ
}

fn native_bool_to_boolean_object(input: bool) -> ObjectTypes {
    if input {
        TRUE
    } else {
        FALSE
    }
}

fn eval_prefix_expression(operator: &str, right: &ObjectTypes) -> ObjectTypes {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => new_error(format!("unknown operator: {}{}", operator, right._type())),
    }
}

fn eval_identifier(node: &Identifier, env: &mut Environment) -> ObjectTypes {
    match env.get(&node.value) {
        Some(val) => val.clone(),
        None => new_error(format!("identifier not found: {}", node.value)),
    }
}

fn eval_bang_operator_expression(right: &ObjectTypes) -> ObjectTypes {
    match right {
        ObjectTypes::Boolean(boolean) => {
            if boolean.value {
                FALSE
            } else {
                TRUE
            }
        }
        ObjectTypes::Null(_) => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: &ObjectTypes) -> ObjectTypes {
    if right._type() != INTEGER_OBJ {
        return new_error(format!("unknown operator: -{}", right._type()));
    }

    match right {
        ObjectTypes::Integer(integer) => ObjectTypes::Integer(Integer {
            value: -integer.value,
        }),
        _ => new_error(format!("unknown operator: -{}", right._type())),
    }
}

fn eval_infix_expression(operator: &str, left: &ObjectTypes, right: &ObjectTypes) -> ObjectTypes {
    if &left._type() == INTEGER_OBJ && &right._type() == INTEGER_OBJ {
        return eval_integer_infix_expression(operator, left, right);
    };
    if &left._type() != &right._type() {
        return new_error(format!(
            "type mismatch: {} {} {}",
            left._type(),
            operator,
            right._type()
        ));
    }
    if operator == "==" {
        return native_bool_to_boolean_object(left == right);
    }
    if operator == "!=" {
        return native_bool_to_boolean_object(left.inspect() != right.inspect());
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
    left: &ObjectTypes,
    right: &ObjectTypes,
) -> ObjectTypes {
    let left_val = match &left {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return new_error("left is not an integer".to_string()),
    };
    let right_val = match &right {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return new_error("right is not an integer".to_string()),
    };

    match operator {
        "+" => ObjectTypes::Integer(Integer {
            value: left_val + right_val,
        }),
        "-" => ObjectTypes::Integer(Integer {
            value: left_val - right_val,
        }),
        "*" => ObjectTypes::Integer(Integer {
            value: left_val * right_val,
        }),
        "/" => ObjectTypes::Integer(Integer {
            value: left_val / right_val,
        }),
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

fn is_truthy(obj: &ObjectTypes) -> bool {
    match obj {
        ObjectTypes::Boolean(boolean) => boolean.value,
        ObjectTypes::Null(_) => false,
        _ => true,
    }
}

fn eval_if_expression(ie: &IfExpression, env: &mut Environment) -> ObjectTypes {
    let condition = eval(ie.condition.as_ref().unwrap(), env);
    if is_truthy(&condition) {
        return eval(ie.consequence.as_ref().unwrap(), env);
    } else if let Some(alt) = &ie.alternative {
        return eval(alt, env);
    }
    return ObjectTypes::Null(Null {});
}
