use std::collections::HashMap;

use ::ast::ast::{
    Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, PrefixExpression,
    Program, ReturnStatement, Statement,
};
use ::lexer::lexer::Lexer;
use lexer::lexer;
use token::token;

type PrefixParseFn = fn(&mut Parser) -> Option<Box<dyn Expression>>;
// type InfixParseFn =
//     fn(&mut Parser, left: Box<dyn Expression>) -> Option<Box<dyn Expression>>;

pub enum Precedence {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
}

pub struct Parser {
    lexer: lexer::Lexer,
    errors: Vec<String>,

    cur_token: token::Token,
    peek_token: token::Token,

    prefix_parse_fns: HashMap<token::TokenType, PrefixParseFn>,
    // infix_parse_fns: HashMap<token::TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            errors: vec![],
            cur_token: token::Token::new(),
            peek_token: token::Token::new(),
            prefix_parse_fns: HashMap::new(),
            // infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(token::IDENT, Self::parse_identifier);
        parser.register_prefix(token::INT, Self::parse_integer_literal);
        parser.register_prefix(token::BANG, Self::parse_prefix_expression);
        parser.register_prefix(token::MINUS, Self::parse_prefix_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn peek_error(&mut self, tok: token::TokenType) {
        let msg = format!(
            "expected next token to be {}, got {} instead",
            tok, self.peek_token.type_
        );
        self.errors.push(msg);
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program { statements: vec![] };

        while self.cur_token.type_ != token::EOF {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                program.statements.push(s);
            }
            self.next_token();
        }
        Some(program)
    }

    pub fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.cur_token.type_ {
            token::LET => self.parse_let_statement(),
            token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: None,
            value: None,
        };

        if !self.expect_peek(token::IDENT) {
            return None;
        }

        stmt.name = Some(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        });

        if !self.expect_peek(token::ASSIGN) {
            return None;
        }

        while !self.cur_token_is(token::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    pub fn parse_return_statement(&mut self) -> Option<Box<dyn Statement>> {
        let stmt = ReturnStatement {
            token: self.cur_token.clone(),
            return_value: None,
        };

        self.next_token();

        //FIXME: implement parsing of return value
        while !self.cur_token_is(token::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    // TODO: Statementを返すのは正しいのか検証する
    pub fn parse_expression_statement(&mut self) -> Option<Box<dyn Statement>> {
        let mut stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: None,
        };

        stmt.expression = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(token::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    pub fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        let prefix = match self.prefix_parse_fns.get(&self.cur_token.type_) {
            Some(p) => p,
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.type_);
                return None;
            }
        };

        let left_exp = prefix(self);
        left_exp
    }

    pub fn parse_identifier(&mut self) -> Option<Box<dyn Expression>> {
        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        Some(Box::new(ident))
    }

    pub fn parse_integer_literal(&mut self) -> Option<Box<dyn Expression>> {
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

        Some(Box::new(ident))
    }

    pub fn parse_prefix_expression(&mut self) -> Option<Box<dyn Expression>> {
        let mut expression = PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            right: None,
        };

        self.next_token();

        expression.right = self.parse_expression(Precedence::PREFIX);

        Some(Box::new(expression))
    }

    fn cur_token_is(&self, tok: token::TokenType) -> bool {
        self.cur_token.type_ == tok
    }

    fn peek_token_is(&self, tok: token::TokenType) -> bool {
        self.peek_token.type_ == tok
    }

    fn expect_peek(&mut self, tok: token::TokenType) -> bool {
        if self.peek_token_is(tok) {
            self.next_token();
            true
        } else {
            self.peek_error(tok);
            false
        }
    }

    fn register_prefix(&mut self, tok: token::TokenType, fn_: PrefixParseFn) {
        self.prefix_parse_fns.insert(tok, fn_);
    }

    fn no_prefix_parse_fn_error(&mut self, tok: token::TokenType) {
        let msg = format!("no prefix parse function for {} found", tok);
        self.errors.push(msg);
    }

    // fn register_infix(&mut self, tok: token::TokenType, fn_: InfixParseFn) {
    //     self.infix_parse_fns.insert(tok, fn_);
    // }
}
