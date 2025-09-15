//! Evaluator for the Monkey programming language

use ast::ast::{Expression, Node, Statement};
use object::object::{Boolean, Integer, Null, ObjectTypes};

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
