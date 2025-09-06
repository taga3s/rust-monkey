use ::lexer::lexer::Lexer;
use ast::ast;
use lexer::lexer;
use token::token;

pub struct Parser {
    lexer: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            cur_token: token::Token::new(),
            peek_token: token::Token::new(),
            errors: vec![],
        };

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

    pub fn parse_program(&mut self) -> Option<ast::Program> {
        let mut program = ast::Program { statements: vec![] };

        while self.cur_token.type_ != token::EOF {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                program.statements.push(s);
            }
            self.next_token();
        }
        Some(program)
    }

    pub fn parse_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        match self.cur_token.type_ {
            token::LET => self.parse_let_statement(),
            token::RETURN => self.parse_return_statement(),
            _ => None,
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        let mut stmt = ast::LetStatement {
            token: self.cur_token.clone(),
            name: None,
            value: None,
        };

        if !self.expect_peek(token::IDENT) {
            return None;
        }

        stmt.name = Some(ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        });

        if !self.expect_peek(token::ASSIGN) {
            return None;
        }

        while self.cur_token_is(token::SEMICOLON) {
            self.next_token();
        }

        Some(Box::new(stmt))
    }

    pub fn parse_return_statement(&mut self) -> Option<Box<dyn ast::Statement>> {
        let stmt = ast::ReturnStatement {
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
}
