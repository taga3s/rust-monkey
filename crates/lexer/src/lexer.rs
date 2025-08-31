//! Lexer for the Monkey programming language

use token::token;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn next_token(&mut self) -> token::Token {
        self.skip_whitespace();

        let mut tok = token::Token::new();

        match self.ch {
            Some('=') => {
                if self.peek_char() == Some('=') {
                    let ch = self.ch.unwrap();
                    self.read_char();
                    tok.type_ = token::EQ;
                    tok.literal = format!("{}{}", ch, self.ch.unwrap());
                } else {
                    tok = new_token(token::ASSIGN, self.ch.unwrap());
                }
            }
            Some(';') => tok = new_token(token::SEMICOLON, self.ch.unwrap()),
            Some(':') => tok = new_token(token::COLON, self.ch.unwrap()),
            Some('(') => tok = new_token(token::LPAREN, self.ch.unwrap()),
            Some(')') => tok = new_token(token::RPAREN, self.ch.unwrap()),
            Some('[') => tok = new_token(token::LBRACKET, self.ch.unwrap()),
            Some(']') => tok = new_token(token::RBRACKET, self.ch.unwrap()),
            Some(',') => tok = new_token(token::COMMA, self.ch.unwrap()),
            Some('+') => tok = new_token(token::PLUS, self.ch.unwrap()),
            Some('-') => tok = new_token(token::MINUS, self.ch.unwrap()),
            Some('!') => {
                if self.peek_char() == Some('=') {
                    let ch = self.ch.unwrap();
                    self.read_char();
                    tok.type_ = token::NOTEQ;
                    tok.literal = format!("{}{}", ch, self.ch.unwrap());
                } else {
                    tok = new_token(token::BANG, self.ch.unwrap());
                }
            }
            Some('*') => tok = new_token(token::ASTERISK, self.ch.unwrap()),
            Some('<') => tok = new_token(token::LT, self.ch.unwrap()),
            Some('>') => tok = new_token(token::GT, self.ch.unwrap()),
            Some('/') => tok = new_token(token::SLASH, self.ch.unwrap()),
            Some('{') => tok = new_token(token::LBRACE, self.ch.unwrap()),
            Some('}') => tok = new_token(token::RBRACE, self.ch.unwrap()),
            Some('"') => {
                tok.type_ = token::STRING;
                tok.literal = self.read_string();
            }

            None => {
                tok.literal = "".to_string();
                tok.type_ = token::EOF;
            }
            _ => {
                if is_letter(self.ch.unwrap()) {
                    tok.literal = self.read_identifier();
                    tok.type_ = token::lookup_ident(&tok.literal);
                    return tok;
                } else if is_digit(self.ch.unwrap()) {
                    tok.type_ = token::INT;
                    tok.literal = self.read_number();
                    return tok;
                }
                tok = new_token(token::ILLEGAL, self.ch.unwrap());
            }
        };

        self.read_char();
        tok
    }

    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn peek_char(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            self.input.chars().nth(self.read_position)
        }
    }

    pub fn skip_whitespace(&mut self) {
        while self.ch == Some(' ')
            || self.ch == Some('\t')
            || self.ch == Some('\n')
            || self.ch == Some('\r')
        {
            self.read_char();
        }
    }

    pub fn read_identifier(&mut self) -> String {
        let position = self.position;
        while is_letter(self.ch.unwrap()) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    pub fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(self.ch.unwrap()) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    pub fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == Some('"') || self.ch == None {
                break;
            }
        }
        self.input[position..self.position].to_string()
    }
}

fn new_token(token_type: token::TokenType, ch: char) -> token::Token {
    token::Token {
        type_: token_type,
        literal: if ch.to_string() == " " {
            "".to_string()
        } else {
            ch.to_string()
        },
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}

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
