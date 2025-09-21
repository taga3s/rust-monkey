//! Evaluator for the Monkey programming language

use ast::ast::{BlockStatement, Expression, Identifier, IfExpression, Node, Program, Statement};
use object::{
    environment::{new_enclosed_environment, Environment},
    object::{
        Array, Boolean, Function, Integer, Null, ObjectTypes, ReturnValue, StringLiteral,
        ARRAY_OBJ, ERROR_OBJ, INTEGER_OBJ, RETURN_VALUE_OBJ, STRING_OBJ,
    },
};

// const NULL: ObjectTypes = ObjectTypes::Null(Null {});
const TRUE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: true });
const FALSE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: false });

pub fn eval(node: &Node, env: &mut Environment) -> ObjectTypes {
    let result = match node {
        Node::Program(program) => eval_program(&program, env),
        Node::Expression(expr) => match expr {
            Expression::IntegerLiteral(il) => ObjectTypes::Integer(Integer { value: il.value }),
            Expression::StringLiteral(sl) => ObjectTypes::StringLiteral(StringLiteral {
                value: sl.value.clone(),
            }),
            Expression::Boolean(boolean) => native_bool_to_boolean_object(boolean.value),
            Expression::ArrayLiteral(al) => {
                let elements = eval_expressions(&al.elements, env);
                if elements.len() == 1 && is_error(&elements[0]) {
                    return elements[0].clone();
                }
                ObjectTypes::Array(Array { elements })
            }
            Expression::IndexExpression(ie) => {
                let left = eval(ie.left.clone().unwrap().as_ref(), env);
                if is_error(&left) {
                    return left;
                }
                let index = eval(ie.index.clone().unwrap().as_ref(), env);
                if is_error(&index) {
                    return index;
                }
                return eval_index_expression(&left, &index);
            }
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
            Expression::FunctionLiteral(fl) => {
                let mut parameters = Vec::new();
                for p in &fl.parameters {
                    match p.as_ref() {
                        Node::Expression(Expression::Identifier(ident)) => {
                            parameters.push(ident.clone())
                        }
                        _ => {
                            return new_error(
                                "function parameter is not an identifier".to_string(),
                            );
                        }
                    }
                }
                let body = match fl.body.as_ref().unwrap().as_ref() {
                    Node::Statement(Statement::BlockStatement(bs)) => bs,
                    _ => {
                        return new_error("function body is not a block statement".to_string());
                    }
                };
                return ObjectTypes::Function(Function {
                    parameters,
                    body: body.clone(),
                    env: env.clone(),
                });
            }
            Expression::CallExpression(ce) => {
                let func = eval(ce.function.as_ref(), env);
                if is_error(&func) {
                    return func;
                }
                let args = eval_expressions(&ce.arguments, env);
                if args.len() == 1 && is_error(&args[0]) {
                    return args[0].clone();
                }
                return apply_function(&func, &args);
            }
        },
        Node::Statement(stmt) => match stmt {
            Statement::ExpressionStatement(es) => eval(es.expression.as_ref().unwrap(), env),
            Statement::Let(ls) => {
                let val = eval(ls.value.as_ref().unwrap(), env);
                if is_error(&val) {
                    return val;
                }
                return env.set(ls.name.as_ref().unwrap().value.clone(), val);
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
    if let Some(val) = env.get(&node.value) {
        return val.clone();
    }

    if let Some((_, builtin)) = crate::builtins::BUILTINS
        .iter()
        .find(|(name, _)| *name == node.value)
    {
        return builtin.clone();
    }

    return new_error(format!("identifier not found: {}", node.value));
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
    if &left._type() == STRING_OBJ && &right._type() == STRING_OBJ {
        return eval_string_infix_expression(operator, left, right);
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

fn eval_string_infix_expression(
    operator: &str,
    left: &ObjectTypes,
    right: &ObjectTypes,
) -> ObjectTypes {
    let left_val = match &left {
        ObjectTypes::StringLiteral(string) => string.value.clone(),
        _ => return new_error("left is not a string".to_string()),
    };
    let right_val = match &right {
        ObjectTypes::StringLiteral(string) => string.value.clone(),
        _ => return new_error("right is not a string".to_string()),
    };

    match operator {
        "+" => ObjectTypes::StringLiteral(StringLiteral {
            value: format!("{}{}", left_val, right_val),
        }),
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

fn eval_expressions(exps: &Vec<Box<Node>>, env: &mut Environment) -> Vec<ObjectTypes> {
    let mut result = Vec::new();

    for e in exps {
        let evaluated = eval(e.as_ref(), env);
        if is_error(&evaluated) {
            return vec![evaluated];
        }
        result.push(evaluated);
    }
    result
}

fn apply_function(func: &ObjectTypes, args: &Vec<ObjectTypes>) -> ObjectTypes {
    if let ObjectTypes::Function(function) = func {
        let mut extended_env = extend_function_env(&function, args);
        let evaluated = eval(
            &Node::Statement(Statement::BlockStatement(function.body.clone())),
            &mut extended_env,
        );
        return unwrap_return_value(evaluated);
    }

    if let ObjectTypes::Builtin(builtin) = func {
        return (builtin._fn)(args.clone());
    }

    new_error(format!("not a function: {}", func._type()))
}

fn extend_function_env(func: &Function, args: &Vec<ObjectTypes>) -> Environment {
    let mut env = new_enclosed_environment(func.env.clone());

    for (param_idx, param) in func.parameters.iter().enumerate() {
        env.set(param.value.clone(), args[param_idx].clone());
    }

    env
}

fn unwrap_return_value(obj: ObjectTypes) -> ObjectTypes {
    if obj._type() == RETURN_VALUE_OBJ {
        if let ObjectTypes::ReturnValue(rv) = obj {
            return *rv.value;
        }
    }
    obj
}

fn eval_index_expression(left: &ObjectTypes, index: &ObjectTypes) -> ObjectTypes {
    if left._type() != ARRAY_OBJ || index._type() != INTEGER_OBJ {
        return new_error(format!("index operator not supported: {}", left._type()));
    }

    eval_array_expression(left, index)
}

fn eval_array_expression(left: &ObjectTypes, index: &ObjectTypes) -> ObjectTypes {
    let array = match left {
        ObjectTypes::Array(array) => array,
        _ => return new_error("left is not an array".to_string()),
    };
    let idx = match index {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return new_error("index is not an integer".to_string()),
    };
    let max = array.elements.len() as i64 - 1;

    if idx < 0 || idx > max {
        return ObjectTypes::Null(Null {});
    }

    array.elements[idx as usize].clone()
}
