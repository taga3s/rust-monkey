//! Parser for the Monkey programming language

use std::{collections::HashMap, vec};

use ast::ast::{
    BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement, FunctionLiteral,
    Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, Node,
    PrefixExpression, Program, ReturnStatement, Statement, StringLiteral,
};
use lexer::lexer::Lexer;
use token::token::{Token, TokenType};

type PrefixParseFn = fn(&mut Parser) -> Option<Box<Node>>;
type InfixParseFn = fn(&mut Parser, left: Box<Node>) -> Option<Box<Node>>;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
}

const PRECEDENCES: [(TokenType, Precedence); 9] = [
    (TokenType::EQ, Precedence::EQUALS),
    (TokenType::NOTEQ, Precedence::EQUALS),
    (TokenType::LT, Precedence::LESSGREATER),
    (TokenType::GT, Precedence::LESSGREATER),
    (TokenType::PLUS, Precedence::SUM),
    (TokenType::MINUS, Precedence::SUM),
    (TokenType::SLASH, Precedence::PRODUCT),
    (TokenType::ASTERISK, Precedence::PRODUCT),
    (TokenType::LPAREN, Precedence::CALL),
];

pub struct Parser {
    lexer: Lexer,
    errors: Vec<String>,

    cur_token: Token,
    peek_token: Token,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            errors: vec![],
            cur_token: Token::new(),
            peek_token: Token::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::IDENT, Self::parse_identifier);
        parser.register_prefix(TokenType::INT, Self::parse_integer_literal);
        parser.register_prefix(TokenType::STRING, Self::parse_string_literal);
        parser.register_prefix(TokenType::TRUE, Self::parse_boolean);
        parser.register_prefix(TokenType::FALSE, Self::parse_boolean);
        parser.register_prefix(TokenType::BANG, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::MINUS, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::LPAREN, Self::parse_grouped_expression);
        parser.register_prefix(TokenType::IF, Self::parse_if_expression);
        parser.register_prefix(TokenType::FUNCTION, Self::parse_function_literal);

        parser.register_infix(TokenType::PLUS, Self::parse_infix_expression);
        parser.register_infix(TokenType::MINUS, Self::parse_infix_expression);
        parser.register_infix(TokenType::SLASH, Self::parse_infix_expression);
        parser.register_infix(TokenType::ASTERISK, Self::parse_infix_expression);
        parser.register_infix(TokenType::EQ, Self::parse_infix_expression);
        parser.register_infix(TokenType::NOTEQ, Self::parse_infix_expression);
        parser.register_infix(TokenType::LT, Self::parse_infix_expression);
        parser.register_infix(TokenType::GT, Self::parse_infix_expression);
        parser.register_infix(TokenType::LPAREN, Self::parse_call_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, tok: TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            tok, self.peek_token._type
        );
        self.errors.push(msg);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Node {
        let mut program = Program { statements: vec![] };

        while self.cur_token._type != TokenType::EOF {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                program.statements.push(s);
            }
            self.next_token();
        }
        Node::Program(program)
    }

    fn parse_statement(&mut self) -> Option<Node> {
        let stmt = match self.cur_token._type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        };
        stmt.map(|s| Node::Statement(s))
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: None,
            value: None,
        };

        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        stmt.name = Some(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        });

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }

        self.next_token();

        stmt.value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::Let(stmt))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let mut stmt = ReturnStatement {
            token: self.cur_token.clone(),
            return_value: None,
        };

        self.next_token();

        stmt.return_value = self.parse_expression(Precedence::LOWEST);

        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::Return(stmt))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(Precedence::LOWEST),
        };

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::ExpressionStatement(stmt))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<Node>> {
        let prefix_parse_fn = match self.prefix_parse_fns.get(&self.cur_token._type) {
            Some(p) => p,
            None => {
                self.no_prefix_parse_fn_error(self.cur_token._type.clone());
                return None;
            }
        };

        let mut left_exp = prefix_parse_fn(self);

        // 右結合力が高い場合、 left_exp が次の演算子に関連づけられている infix_parse_fn に渡されることはない。
        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let infix_parse_fn = match self.infix_parse_fns.get(&self.peek_token._type) {
                Some(f) => f.clone(),
                None => return left_exp,
            };

            self.next_token();

            left_exp = match left_exp {
                Some(le) => infix_parse_fn(self, le),
                None => return None,
            };
        }

        left_exp
    }

    fn register_prefix(&mut self, tok: TokenType, _fn: PrefixParseFn) {
        self.prefix_parse_fns.insert(tok, _fn);
    }

    fn no_prefix_parse_fn_error(&mut self, tok: TokenType) {
        let msg = format!("no prefix parse function for {:?} found", tok);
        self.errors.push(msg);
    }

    fn parse_identifier(&mut self) -> Option<Box<Node>> {
        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        Some(Box::new(Node::Expression(Expression::Identifier(ident))))
    }

    fn parse_integer_literal(&mut self) -> Option<Box<Node>> {
        let value = match self.cur_token.literal.parse::<i64>() {
            Ok(v) => v,
            Err(_) => {
                let msg = format!("could not parse {} as integer", self.cur_token.literal);
                self.errors.push(msg);
                return None;
            }
        };

        let ident = IntegerLiteral {
            token: self.cur_token.clone(),
            value,
        };

        Some(Box::new(Node::Expression(Expression::IntegerLiteral(
            ident,
        ))))
    }

    fn parse_string_literal(&mut self) -> Option<Box<Node>> {
        Some(Box::new(Node::Expression(Expression::StringLiteral(
            StringLiteral {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            },
        ))))
    }

    fn parse_boolean(&mut self) -> Option<Box<Node>> {
        let boolean = Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::TRUE),
        };

        Some(Box::new(Node::Expression(Expression::Boolean(boolean))))
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<Node>> {
        let mut expression = PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            right: None,
        };

        self.next_token();

        expression.right = self.parse_expression(Precedence::PREFIX);

        Some(Box::new(Node::Expression(Expression::Prefix(expression))))
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<Node>> {
        self.next_token();

        let exp = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        exp
    }

    fn register_infix(&mut self, tok: TokenType, _fn: InfixParseFn) {
        self.infix_parse_fns.insert(tok, _fn);
    }

    fn parse_infix_expression(&mut self, left: Box<Node>) -> Option<Box<Node>> {
        let mut expression = InfixExpression {
            token: self.cur_token.clone(),
            left: Some(left),
            operator: self.cur_token.literal.clone(),
            right: None,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        expression.right = self.parse_expression(precedence);

        Some(Box::new(Node::Expression(Expression::Infix(expression))))
    }

    fn parse_function_literal(&mut self) -> Option<Box<Node>> {
        let mut lit = FunctionLiteral {
            token: self.cur_token.clone(),
            parameters: vec![],
            body: None,
        };

        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        lit.parameters = match self.parse_function_parameters() {
            Some(p) => p,
            None => return None,
        };

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        lit.body = self.parse_block_statement();

        Some(Box::new(Node::Expression(Expression::FunctionLiteral(lit))))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Box<Node>>> {
        let mut identifiers: Vec<Box<Node>> = vec![];

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        identifiers.push(Box::new(Node::Expression(Expression::Identifier(ident))));

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };
            identifiers.push(Box::new(Node::Expression(Expression::Identifier(ident))));
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_if_expression(&mut self) -> Option<Box<Node>> {
        let mut expression = IfExpression {
            token: self.cur_token.clone(),
            condition: None,
            consequence: None,
            alternative: None,
        };

        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        self.next_token();
        expression.condition = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        expression.consequence = self.parse_block_statement();

        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();

            if !self.expect_peek(TokenType::LBRACE) {
                return None;
            }

            expression.alternative = self.parse_block_statement();
        }

        Some(Box::new(Node::Expression(Expression::IfExpression(
            expression,
        ))))
    }

    fn parse_block_statement(&mut self) -> Option<Box<Node>> {
        let mut block = BlockStatement {
            token: self.cur_token.clone(),
            statements: vec![],
        };

        self.next_token();

        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            if let Some(stmt) = self.parse_statement() {
                block.statements.push(stmt);
            }
            self.next_token();
        }

        Some(Box::new(Node::Statement(Statement::BlockStatement(block))))
    }

    fn parse_call_expression(&mut self, function: Box<Node>) -> Option<Box<Node>> {
        let exp = CallExpression {
            token: self.cur_token.clone(),
            function,
            arguments: self.parse_call_arguments()?,
        };
        Some(Box::new(Node::Expression(Expression::CallExpression(exp))))
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<Node>>> {
        let mut args = vec![];

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::LOWEST)?);
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        Some(args)
    }

    fn cur_token_is(&self, tok: TokenType) -> bool {
        self.cur_token._type == tok
    }

    fn peek_token_is(&self, tok: TokenType) -> bool {
        self.peek_token._type == tok
    }

    fn expect_peek(&mut self, tok: TokenType) -> bool {
        if self.peek_token_is(tok.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(tok);
            false
        }
    }

    fn peek_precedence(&self) -> Precedence {
        match PRECEDENCES
            .iter()
            .find(|(t, _)| t.clone() == self.peek_token._type)
        {
            Some((_, p)) => p.clone(),
            None => Precedence::LOWEST,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match PRECEDENCES.iter().find(|(t, _)| t == &self.cur_token._type) {
            Some((_, p)) => p.clone(),
            None => Precedence::LOWEST,
        }
    }
}
