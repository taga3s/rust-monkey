//! AST for the Monkey programming language
use std::collections::HashMap;

use token::token::Token;

use std::hash::{Hash, Hasher};

#[derive(PartialEq, Clone, Debug)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl Node {
    pub fn to_string(&self) -> String {
        match self {
            Node::Program(p) => p.to_string(),
            Node::Statement(s) => s.to_string(),
            Node::Expression(e) => e.to_string(),
        }
    }
}

impl Eq for Node {}

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state); //FIXME: This is not a good hash function
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Statement {
    pub fn to_string(&self) -> String {
        match self {
            Statement::Let(s) => s.to_string(),
            Statement::Return(s) => s.to_string(),
            Statement::ExpressionStatement(s) => s.to_string(),
            Statement::BlockStatement(s) => s.to_string(),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    Boolean(Boolean),
    ArrayLiteral(ArrayLiteral),
    IndexExpression(IndexExpression),
    Identifier(Identifier),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    HashLiteral(HashLiteral),
}

impl Expression {
    pub fn to_string(&self) -> String {
        match self {
            Expression::IntegerLiteral(e) => e.to_string(),
            Expression::StringLiteral(e) => e.to_string(),
            Expression::Boolean(e) => e.to_string(),
            Expression::ArrayLiteral(e) => e.to_string(),
            Expression::IndexExpression(e) => e.to_string(),
            Expression::Identifier(e) => e.to_string(),
            Expression::Prefix(e) => e.to_string(),
            Expression::Infix(e) => e.to_string(),
            Expression::IfExpression(e) => e.to_string(),
            Expression::FunctionLiteral(e) => e.to_string(),
            Expression::CallExpression(e) => e.to_string(),
            Expression::HashLiteral(e) => e.to_string(),
        }
    }
}

pub trait TNode {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait TStatement
where
    Self: TNode,
{
    fn statement_node(&self);
}

pub trait TExpression
where
    Self: TNode,
{
    fn expression_node(&self);
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program {
    pub statements: Vec<Node>,
}

impl TNode for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            String::from(self.statements[0].to_string())
        } else {
            "".to_string()
        }
    }

    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl TExpression for Identifier {
    fn expression_node(&self) {}
}

impl TNode for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>,
    pub value: Option<Box<Node>>,
}

impl TStatement for LetStatement {
    fn statement_node(&self) {}
}

impl TNode for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        format!(
            "{} {} = {};",
            self.token_literal(),
            self.name.as_ref().map_or("".to_string(), |n| n.to_string()),
            self.value
                .as_ref()
                .map_or("".to_string(), |v| v.to_string())
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<Node>>,
}

impl TStatement for ReturnStatement {
    fn statement_node(&self) {}
}

impl TNode for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        format!(
            "{} {};",
            self.token_literal(),
            self.return_value
                .as_ref()
                .map_or("".to_string(), |v| v.to_string())
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<Node>>,
}

impl TStatement for ExpressionStatement {
    fn statement_node(&self) {}
}

impl TNode for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        self.expression
            .as_ref()
            .map_or("".to_string(), |e| e.to_string())
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl TExpression for IntegerLiteral {
    fn expression_node(&self) {}
}

impl TNode for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl TExpression for StringLiteral {
    fn expression_node(&self) {}
}

impl TNode for StringLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Option<Box<Node>>,
}

impl TExpression for PrefixExpression {
    fn expression_node(&self) {}
}

impl TNode for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        format!(
            "({}{})",
            self.operator,
            self.right
                .as_ref()
                .map_or("".to_string(), |r| r.to_string())
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Option<Box<Node>>,
    pub operator: String,
    pub right: Option<Box<Node>>,
}

impl TExpression for InfixExpression {
    fn expression_node(&self) {}
}

impl TNode for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        format!(
            "({} {} {})",
            self.left.as_ref().map_or("".to_string(), |l| l.to_string()),
            self.operator,
            self.right
                .as_ref()
                .map_or("".to_string(), |r| r.to_string())
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl TExpression for Boolean {
    fn expression_node(&self) {}
}

impl TNode for Boolean {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Box<Node>>,
}

impl TExpression for ArrayLiteral {
    fn expression_node(&self) {}
}

impl TNode for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        let elems = self
            .elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("[{}]", elems)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Option<Box<Node>>,
    pub index: Option<Box<Node>>,
}

impl TExpression for IndexExpression {
    fn expression_node(&self) {}
}

impl TNode for IndexExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        format!(
            "({}[{}])",
            self.left.as_ref().map_or("".to_string(), |l| l.to_string()),
            self.index
                .as_ref()
                .map_or("".to_string(), |i| i.to_string())
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Option<Box<Node>>,
    pub consequence: Option<Box<Node>>,
    pub alternative: Option<Box<Node>>,
}

impl TExpression for IfExpression {
    fn expression_node(&self) {}
}

impl TNode for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        format!(
            "if{}{}{}",
            self.condition
                .as_ref()
                .map_or("".to_string(), |c| c.to_string()),
            self.consequence
                .as_ref()
                .map_or("".to_string(), |c| c.to_string()),
            self.alternative
                .as_ref()
                .map_or("".to_string(), |a| format!("else{}", a.to_string()))
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Node>,
}

impl TStatement for BlockStatement {
    fn statement_node(&self) {}
}

impl TNode for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Box<Node>>,
    pub body: Option<Box<Node>>,
}

impl TExpression for FunctionLiteral {
    fn expression_node(&self) {}
}

impl TNode for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        let params = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let body = self.body.as_ref().map_or("".to_string(), |b| b.to_string());
        format!("{}({}){{{}}}", self.token_literal(), params, body)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Node>,
    pub arguments: Vec<Box<Node>>,
}

impl TExpression for CallExpression {
    fn expression_node(&self) {}
}

impl TNode for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        let func = &self.function.to_string();
        let args = self
            .arguments
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        format!("{}({})", func, args)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: HashMap<Box<Node>, Box<Node>>,
}

impl TExpression for HashLiteral {
    fn expression_node(&self) {}
}

impl TNode for HashLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.to_string()
    }

    fn to_string(&self) -> String {
        let pairs = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{}: {}", k.to_string(), v.to_string()))
            .collect::<Vec<String>>()
            .join(", ");
        format!("{{{}}}", pairs)
    }
}
