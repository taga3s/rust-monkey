use std::any::Any;

use token::token;

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait Statement
where
    Self: Node + AsAny,
{
    fn statement_node(&self);
}

pub trait Expression
where
    Self: Node + AsAny,
{
    fn expression_node(&self);
}

pub trait AsAny {
    fn as_any(&self) -> &dyn Any;
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

    pub fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}

pub struct Identifier {
    pub token: token::Token,
    pub value: String,
}

impl Expression for Identifier {
    fn expression_node(&self) {}
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }
}

impl AsAny for Identifier {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct LetStatement {
    pub token: token::Token,
    pub name: Option<Identifier>,
    pub value: Option<Box<dyn Expression>>,
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token_literal());
        out.push(' ');
        if let Some(name) = &self.name {
            out.push_str(&name.to_string());
        }
        out.push_str(" = ");
        if let Some(value) = &self.value {
            out.push_str(&value.to_string());
        }
        out.push(';');
        out
    }
}

impl AsAny for LetStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct ReturnStatement {
    pub token: token::Token,
    pub return_value: Option<Box<dyn Expression>>,
}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token_literal());
        out.push(' ');

        if let Some(value) = &self.return_value {
            out.push_str(&value.to_string());
        }
        out.push(';');
        out
    }
}

impl AsAny for ReturnStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        if let Some(expression) = &self.expression {
            expression.to_string()
        } else {
            "".to_string()
        }
    }
}

impl AsAny for ExpressionStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct IntegerLiteral {
    pub token: token::Token,
    pub value: i64,
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) {}
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

impl AsAny for IntegerLiteral {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct PrefixExpression {
    pub token: token::Token,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl Expression for PrefixExpression {
    fn expression_node(&self) {}
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push('(');
        out.push_str(&self.operator);
        if let Some(right) = &self.right {
            out.push_str(&right.to_string());
        }
        out.push(')');
        out
    }
}

impl AsAny for PrefixExpression {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct InfixExpression {
    pub token: token::Token,
    pub left: Option<Box<dyn Expression>>,
    pub operator: String,
    pub right: Option<Box<dyn Expression>>,
}

impl Expression for InfixExpression {
    fn expression_node(&self) {}
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push('(');
        if let Some(left) = &self.left {
            out.push_str(&left.to_string());
        }
        out.push(' ');
        out.push_str(&self.operator);
        out.push(' ');
        if let Some(right) = &self.right {
            out.push_str(&right.to_string());
        }
        out.push(')');
        out
    }
}

impl AsAny for InfixExpression {
    fn as_any(&self) -> &dyn Any {
        self
    }
}
