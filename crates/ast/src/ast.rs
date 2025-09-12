//! AST for the Monkey programming language
use token::token;

pub enum StatementTypes {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

pub enum ExpressionTypes {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
}

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait Statement
where
    Self: Node,
{
    fn statement_node(&self);
}

pub trait Expression
where
    Self: Node,
{
    fn expression_node(&self);
}

pub struct Program {
    pub statements: Vec<StatementTypes>,
}

impl Program {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            match &self.statements[0] {
                StatementTypes::LetStatement(s) => s.token_literal(),
                StatementTypes::ReturnStatement(s) => s.token_literal(),
                StatementTypes::ExpressionStatement(s) => s.token_literal(),
            }
        } else {
            "".to_string()
        }
    }

    pub fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|s| match s {
                StatementTypes::LetStatement(stmt) => stmt.to_string(),
                StatementTypes::ReturnStatement(stmt) => stmt.to_string(),
                StatementTypes::ExpressionStatement(stmt) => stmt.to_string(),
            })
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

pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Option<Box<ExpressionTypes>>,
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
            match expression.as_ref() {
                ExpressionTypes::Identifier(expr) => expr.to_string(),
                ExpressionTypes::IntegerLiteral(expr) => expr.to_string(),
                ExpressionTypes::PrefixExpression(expr) => expr.to_string(),
                ExpressionTypes::InfixExpression(expr) => expr.to_string(),
                ExpressionTypes::Boolean(expr) => expr.to_string(),
            }
        } else {
            "".to_string()
        }
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

pub struct PrefixExpression {
    pub token: token::Token,
    pub operator: String,
    pub right: Option<Box<ExpressionTypes>>,
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
            match right.as_ref() {
                ExpressionTypes::Identifier(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::IntegerLiteral(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::PrefixExpression(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::InfixExpression(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::Boolean(expr) => out.push_str(&expr.to_string()),
            }
        }
        out.push(')');
        out
    }
}

pub struct InfixExpression {
    pub token: token::Token,
    pub left: Option<Box<ExpressionTypes>>,
    pub operator: String,
    pub right: Option<Box<ExpressionTypes>>,
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
            match left.as_ref() {
                ExpressionTypes::Identifier(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::IntegerLiteral(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::PrefixExpression(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::InfixExpression(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::Boolean(expr) => out.push_str(&expr.to_string()),
            }
        }
        out.push(' ');
        out.push_str(&self.operator);
        out.push(' ');
        if let Some(right) = &self.right {
            match right.as_ref() {
                ExpressionTypes::Identifier(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::IntegerLiteral(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::PrefixExpression(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::InfixExpression(expr) => out.push_str(&expr.to_string()),
                ExpressionTypes::Boolean(expr) => out.push_str(&expr.to_string()),
            }
        }
        out.push(')');
        out
    }
}

pub struct Boolean {
    pub token: token::Token,
    pub value: bool,
}

impl Expression for Boolean {
    fn expression_node(&self) {}
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}
