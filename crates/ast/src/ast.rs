//! AST for the Monkey programming language
use token::token;

pub enum StatementTypes {
    Let(LetStatement),
    Return(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

fn to_string_from_statement_types(stmt: &StatementTypes) -> String {
    match stmt {
        StatementTypes::Let(s) => s.to_string(),
        StatementTypes::Return(s) => s.to_string(),
        StatementTypes::ExpressionStatement(s) => s.to_string(),
    }
}

pub enum ExpressionTypes {
    IntegerLiteral(IntegerLiteral),
    Boolean(Boolean),
    Identifier(Identifier),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

fn to_string_from_expression_types(expr: &ExpressionTypes) -> String {
    match expr {
        ExpressionTypes::Identifier(ident) => ident.to_string(),
        ExpressionTypes::IntegerLiteral(int_lit) => int_lit.to_string(),
        ExpressionTypes::Prefix(prefix_expr) => prefix_expr.to_string(),
        ExpressionTypes::Infix(infix_expr) => infix_expr.to_string(),
        ExpressionTypes::Boolean(bool_expr) => bool_expr.to_string(),
        ExpressionTypes::IfExpression(if_expr) => if_expr.to_string(),
        ExpressionTypes::FunctionLiteral(func_lit) => func_lit.to_string(),
        ExpressionTypes::CallExpression(call_expr) => call_expr.to_string(),
    }
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
            to_string_from_statement_types(&self.statements[0])
        } else {
            "".to_string()
        }
    }

    pub fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|s| to_string_from_statement_types(s))
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
    pub value: Option<Box<ExpressionTypes>>,
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
            out.push_str(&to_string_from_expression_types(value));
        }
        out.push(';');
        out
    }
}

pub struct ReturnStatement {
    pub token: token::Token,
    pub return_value: Option<Box<ExpressionTypes>>,
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
            out.push_str(&to_string_from_expression_types(value));
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
            to_string_from_expression_types(expression)
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
            out.push_str(&to_string_from_expression_types(right));
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
            out.push_str(&to_string_from_expression_types(left));
        }
        out.push(' ');
        out.push_str(&self.operator);
        out.push(' ');
        if let Some(right) = &self.right {
            out.push_str(&to_string_from_expression_types(right));
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

pub struct IfExpression {
    pub token: token::Token,
    pub condition: Option<Box<ExpressionTypes>>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
}

impl Expression for IfExpression {
    fn expression_node(&self) {}
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push_str("if");
        if let Some(condition) = &self.condition {
            out.push_str(&to_string_from_expression_types(condition));
        }
        if let Some(consequence) = &self.consequence {
            out.push_str(&consequence.to_string());
        }
        if let Some(alternative) = &self.alternative {
            out.push_str("else");
            out.push_str(&alternative.to_string());
        }
        out
    }
}

pub struct BlockStatement {
    pub token: token::Token,
    pub statements: Vec<StatementTypes>,
}

impl Statement for BlockStatement {
    fn statement_node(&self) {}
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        for statement in &self.statements {
            out.push_str(&to_string_from_statement_types(statement));
        }
        out
    }
}

pub struct FunctionLiteral {
    pub token: token::Token,
    pub parameters: Vec<Box<ExpressionTypes>>,
    pub body: Option<BlockStatement>,
}

impl Expression for FunctionLiteral {
    fn expression_node(&self) {}
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token_literal());
        out.push('(');
        let params = self
            .parameters
            .iter()
            .map(|p| match p.as_ref() {
                ExpressionTypes::Identifier(ident) => ident.to_string(),
                _ => "".to_string(),
            })
            .collect::<Vec<String>>()
            .join(", ");
        out.push_str(&params);
        out.push(')');
        out.push('{');
        if let Some(body) = &self.body {
            out.push_str(&body.to_string());
        }
        out.push('}');
        out
    }
}

pub struct CallExpression {
    pub token: token::Token,
    pub function: Box<ExpressionTypes>,
    pub arguments: Vec<Box<ExpressionTypes>>,
}

impl Expression for CallExpression {
    fn expression_node(&self) {}
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        out.push_str(&to_string_from_expression_types(&self.function));
        out.push('(');
        let args = self
            .arguments
            .iter()
            .map(|a| to_string_from_expression_types(a))
            .collect::<Vec<String>>()
            .join(", ");
        out.push_str(&args);
        out.push(')');
        out
    }
}
