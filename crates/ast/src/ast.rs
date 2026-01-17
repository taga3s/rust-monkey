//! AST for the Monkey programming language
use std::{collections::HashMap, fmt};

use token::token::Token;

use std::hash::{Hash, Hasher};

#[derive(PartialEq, Clone, Debug)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Program(p) => write!(f, "{}", p),
            Node::Statement(s) => write!(f, "{}", s),
            Node::Expression(e) => write!(f, "{}", e),
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

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(s) => write!(f, "{}", s),
            Statement::Return(s) => write!(f, "{}", s),
            Statement::ExpressionStatement(s) => write!(f, "{}", s),
            Statement::BlockStatement(s) => write!(f, "{}", s),
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::IntegerLiteral(e) => write!(f, "{}", e),
            Expression::StringLiteral(e) => write!(f, "{}", e),
            Expression::Boolean(e) => write!(f, "{}", e),
            Expression::ArrayLiteral(e) => write!(f, "{}", e),
            Expression::IndexExpression(e) => write!(f, "{}", e),
            Expression::Identifier(e) => write!(f, "{}", e),
            Expression::Prefix(e) => write!(f, "{}", e),
            Expression::Infix(e) => write!(f, "{}", e),
            Expression::IfExpression(e) => write!(f, "{}", e),
            Expression::FunctionLiteral(e) => write!(f, "{}", e),
            Expression::CallExpression(e) => write!(f, "{}", e),
            Expression::HashLiteral(e) => write!(f, "{}", e),
        }
    }
}

pub trait TNode {
    fn token_literal(&self) -> String;
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
        if !self.statements.is_empty() {
            self.statements[0].to_string()
        } else {
            "".to_string()
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = self
            .statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("");
        write!(f, "{}", out)
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
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
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
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "{} {} = {};",
            self.token_literal(),
            self.name.as_ref().map_or("".to_string(), |n| n.to_string()),
            self.value
                .as_ref()
                .map_or("".to_string(), |v| v.to_string())
        );
        write!(f, "{}", out)
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
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "{} {};",
            self.token_literal(),
            self.return_value
                .as_ref()
                .map_or("".to_string(), |v| v.to_string())
        );
        write!(f, "{}", out)
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
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.expression
                .as_ref()
                .map_or("".to_string(), |e| e.to_string())
        )
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
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
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
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
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
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "({}{})",
            self.operator,
            self.right
                .as_ref()
                .map_or("".to_string(), |r| r.to_string())
        );
        write!(f, "{}", out)
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
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "({} {} {})",
            self.left.as_ref().map_or("".to_string(), |l| l.to_string()),
            self.operator,
            self.right
                .as_ref()
                .map_or("".to_string(), |r| r.to_string())
        );
        write!(f, "{}", out)
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
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
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
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = self
            .elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", out)
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
}

impl fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "({}[{}])",
            self.left.as_ref().map_or("".to_string(), |l| l.to_string()),
            self.index
                .as_ref()
                .map_or("".to_string(), |i| i.to_string())
        );
        write!(f, "{}", out)
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
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "if{}{}{}",
            self.condition
                .as_ref()
                .map_or("".to_string(), |c| c.to_string()),
            self.consequence
                .as_ref()
                .map_or("".to_string(), |c| c.to_string()),
            self.alternative
                .as_ref()
                .map_or("".to_string(), |a| format!("else{}", a))
        );
        write!(f, "{}", out)
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
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = self
            .statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("");
        write!(f, "{}", out)
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
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let body = self.body.as_ref().map_or("".to_string(), |b| b.to_string());
        let out = format!("{}({}){{{}}}", self.token_literal(), params, body);
        write!(f, "{}", out)
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
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let func = &self.function.to_string();
        let args = self
            .arguments
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let out = format!("{}({})", func, args);
        write!(f, "{}", out)
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
}

impl fmt::Display for HashLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pairs = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v))
            .collect::<Vec<String>>()
            .join(", ");
        let out = format!("{{{}}}", pairs);
        write!(f, "{}", out)
    }
}
