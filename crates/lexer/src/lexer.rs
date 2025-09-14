//! Lexer for the Monkey programming language

use ::token::token::{lookup_ident, Token, TokenType};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespaces();

        let mut tok = Token::new();

        match self.ch {
            Some('=') => {
                if self.peek_char() == Some('=') {
                    let ch = self.ch.unwrap();
                    self.read_char();
                    tok.type_ = TokenType::EQ;
                    tok.literal = format!("{}{}", ch, self.ch.unwrap());
                } else {
                    tok = self.new_token(TokenType::ASSIGN, self.ch.unwrap());
                }
            }
            Some(';') => tok = self.new_token(TokenType::SEMICOLON, self.ch.unwrap()),
            Some(':') => tok = self.new_token(TokenType::COLON, self.ch.unwrap()),
            Some('(') => tok = self.new_token(TokenType::LPAREN, self.ch.unwrap()),
            Some(')') => tok = self.new_token(TokenType::RPAREN, self.ch.unwrap()),
            Some('[') => tok = self.new_token(TokenType::LBRACKET, self.ch.unwrap()),
            Some(']') => tok = self.new_token(TokenType::RBRACKET, self.ch.unwrap()),
            Some(',') => tok = self.new_token(TokenType::COMMA, self.ch.unwrap()),
            Some('+') => tok = self.new_token(TokenType::PLUS, self.ch.unwrap()),
            Some('-') => tok = self.new_token(TokenType::MINUS, self.ch.unwrap()),
            Some('!') => {
                if self.peek_char() == Some('=') {
                    let ch = self.ch.unwrap();
                    self.read_char();
                    tok.type_ = TokenType::NOTEQ;
                    tok.literal = format!("{}{}", ch, self.ch.unwrap());
                } else {
                    tok = self.new_token(TokenType::BANG, self.ch.unwrap());
                }
            }
            Some('*') => tok = self.new_token(TokenType::ASTERISK, self.ch.unwrap()),
            Some('<') => tok = self.new_token(TokenType::LT, self.ch.unwrap()),
            Some('>') => tok = self.new_token(TokenType::GT, self.ch.unwrap()),
            Some('/') => tok = self.new_token(TokenType::SLASH, self.ch.unwrap()),
            Some('{') => tok = self.new_token(TokenType::LBRACE, self.ch.unwrap()),
            Some('}') => tok = self.new_token(TokenType::RBRACE, self.ch.unwrap()),
            Some('"') => {
                tok.type_ = TokenType::STRING;
                tok.literal = self.read_string();
            }
            None => {
                tok.literal = "".to_string();
                tok.type_ = TokenType::EOF;
            }
            _ => {
                if self.is_letter(self.ch.unwrap()) {
                    tok.literal = self.read_identifier();
                    tok.type_ = lookup_ident(&tok.literal);
                    return tok;
                } else if self.is_digit(self.ch.unwrap()) {
                    tok.type_ = TokenType::INT;
                    tok.literal = self.read_number();
                    return tok;
                }
                tok = self.new_token(TokenType::ILLEGAL, self.ch.unwrap());
            }
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            self.input.chars().nth(self.read_position)
        }
    }

    fn skip_whitespaces(&mut self) {
        while self.ch == Some(' ')
            || self.ch == Some('\t')
            || self.ch == Some('\n')
            || self.ch == Some('\r')
        {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        loop {
            let cur_ch = match self.ch {
                Some(c) => c,
                None => break,
            };
            if !self.is_letter(cur_ch) {
                break;
            }
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        loop {
            let cur_ch = match self.ch {
                Some(c) => c,
                None => break,
            };
            if !self.is_digit(cur_ch) {
                break;
            }
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == Some('"') || self.ch == None {
                break;
            }
        }
        self.input[position..self.position].to_string()
    }

    fn new_token(&self, token_type: TokenType, ch: char) -> Token {
        Token {
            type_: token_type,
            literal: if ch.to_string() == " " {
                "".to_string()
            } else {
                ch.to_string()
            },
        }
    }

    fn is_letter(&self, ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn is_digit(&self, ch: char) -> bool {
        ch.is_digit(10)
    }
}
