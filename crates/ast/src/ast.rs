use std::any::Any;

use token::token;

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement
where
    Self: Node,
{
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression
where
    Self: Node,
{
    fn expression_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}

pub struct LetStatement {
    pub token: token::Token,
    pub name: Option<Identifier>,
    pub value: Option<Box<dyn Expression>>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct ReturnStatement {
    pub token: token::Token,
    pub return_value: Option<Box<dyn Expression>>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: token::Token,
    pub value: String,
}

impl Identifier {
    pub fn expression_node(&self) {}

    pub fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}
