//! Parser for the Monkey programming language

use std::collections::HashMap;

use ast::ast::{
    BlockStatement, Boolean, ExpressionStatement, ExpressionTypes, Identifier, IfExpression,
    InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement,
    StatementTypes,
};
use lexer::lexer::Lexer;
use token::token::{Token, TokenType};

type PrefixParseFn = fn(&mut Parser) -> Option<Box<ExpressionTypes>>;
type InfixParseFn = fn(&mut Parser, left: Box<ExpressionTypes>) -> Option<Box<ExpressionTypes>>;

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

const PRECEDENCES: [(TokenType, Precedence); 8] = [
    (TokenType::EQ, Precedence::EQUALS),
    (TokenType::NOTEQ, Precedence::EQUALS),
    (TokenType::LT, Precedence::LESSGREATER),
    (TokenType::GT, Precedence::LESSGREATER),
    (TokenType::PLUS, Precedence::SUM),
    (TokenType::MINUS, Precedence::SUM),
    (TokenType::SLASH, Precedence::PRODUCT),
    (TokenType::ASTERISK, Precedence::PRODUCT),
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
        parser.register_prefix(TokenType::TRUE, Self::parse_boolean);
        parser.register_prefix(TokenType::FALSE, Self::parse_boolean);
        parser.register_prefix(TokenType::BANG, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::MINUS, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::LPAREN, Self::parse_grouped_expression);
        parser.register_prefix(TokenType::IF, Self::parse_if_expression);

        parser.register_infix(TokenType::PLUS, Self::parse_infix_expression);
        parser.register_infix(TokenType::MINUS, Self::parse_infix_expression);
        parser.register_infix(TokenType::SLASH, Self::parse_infix_expression);
        parser.register_infix(TokenType::ASTERISK, Self::parse_infix_expression);
        parser.register_infix(TokenType::EQ, Self::parse_infix_expression);
        parser.register_infix(TokenType::NOTEQ, Self::parse_infix_expression);
        parser.register_infix(TokenType::LT, Self::parse_infix_expression);
        parser.register_infix(TokenType::GT, Self::parse_infix_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn peek_error(&mut self, tok: TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            tok, self.peek_token.type_
        );
        self.errors.push(msg);
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token.type_ != TokenType::EOF {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                program.statements.push(s);
            }
            self.next_token();
        }
        program
    }

    pub fn parse_statement(&mut self) -> Option<StatementTypes> {
        match self.cur_token.type_ {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<StatementTypes> {
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

        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(StatementTypes::LetStatement(stmt))
    }

    pub fn parse_return_statement(&mut self) -> Option<StatementTypes> {
        let stmt = ReturnStatement {
            token: self.cur_token.clone(),
            return_value: None,
        };

        self.next_token();

        //FIXME: implement parsing of return value
        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(StatementTypes::ReturnStatement(stmt))
    }

    pub fn parse_expression_statement(&mut self) -> Option<StatementTypes> {
        let mut stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: None,
        };

        stmt.expression = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(StatementTypes::ExpressionStatement(stmt))
    }

    pub fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<ExpressionTypes>> {
        let prefix_parse_fn = match self.prefix_parse_fns.get(&self.cur_token.type_) {
            Some(p) => *p,
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.type_.clone());
                return None;
            }
        };

        let mut left_exp = prefix_parse_fn(self);

        // 右結合力が高い場合、 left_exp が次の演算子に関連づけられている infix_parse_fn に渡されることはない。
        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let infix_parse_fn = match self.infix_parse_fns.get(&self.peek_token.type_) {
                Some(i) => *i,
                None => return left_exp,
            };

            self.next_token();

            left_exp = infix_parse_fn(self, left_exp.unwrap());
        }

        left_exp
    }

    fn register_prefix(&mut self, tok: TokenType, fn_: PrefixParseFn) {
        self.prefix_parse_fns.insert(tok, fn_);
    }

    fn no_prefix_parse_fn_error(&mut self, tok: TokenType) {
        let msg = format!("no prefix parse function for {:?} found", tok);
        self.errors.push(msg);
    }

    pub fn parse_identifier(&mut self) -> Option<Box<ExpressionTypes>> {
        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        Some(Box::new(ExpressionTypes::Identifier(ident)))
    }

    pub fn parse_integer_literal(&mut self) -> Option<Box<ExpressionTypes>> {
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

        Some(Box::new(ExpressionTypes::IntegerLiteral(ident)))
    }

    pub fn parse_boolean(&mut self) -> Option<Box<ExpressionTypes>> {
        let boolean = Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::TRUE),
        };

        Some(Box::new(ExpressionTypes::Boolean(boolean)))
    }

    pub fn parse_prefix_expression(&mut self) -> Option<Box<ExpressionTypes>> {
        let mut expression = PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            right: None,
        };

        self.next_token();

        expression.right = self.parse_expression(Precedence::PREFIX);

        Some(Box::new(ExpressionTypes::PrefixExpression(expression)))
    }

    pub fn parse_grouped_expression(&mut self) -> Option<Box<ExpressionTypes>> {
        self.next_token();

        let exp = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }

        exp
    }

    fn register_infix(&mut self, tok: TokenType, fn_: InfixParseFn) {
        self.infix_parse_fns.insert(tok, fn_);
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<ExpressionTypes>,
    ) -> Option<Box<ExpressionTypes>> {
        let mut expression = InfixExpression {
            token: self.cur_token.clone(),
            left: Some(left),
            operator: self.cur_token.literal.clone(),
            right: None,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        expression.right = self.parse_expression(precedence);

        Some(Box::new(ExpressionTypes::InfixExpression(expression)))
    }

    fn parse_if_expression(&mut self) -> Option<Box<ExpressionTypes>> {
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

        return Some(Box::new(ExpressionTypes::IfExpression(expression)));
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
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

        Some(block)
    }

    fn cur_token_is(&self, tok: TokenType) -> bool {
        self.cur_token.type_ == tok
    }

    fn peek_token_is(&self, tok: TokenType) -> bool {
        self.peek_token.type_ == tok
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
            .find(|(t, _)| *t == self.peek_token.type_)
        {
            Some((_, p)) => p.clone(),
            None => Precedence::LOWEST,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match PRECEDENCES.iter().find(|(t, _)| *t == self.cur_token.type_) {
            Some((_, p)) => p.clone(),
            None => Precedence::LOWEST,
        }
    }
}
