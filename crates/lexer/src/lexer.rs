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

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let tests = vec![
            (token::ASSIGN, "="),
            (token::PLUS, "+"),
            (token::LPAREN, "("),
            (token::RPAREN, ")"),
            (token::LBRACE, "{"),
            (token::RBRACE, "}"),
            (token::COMMA, ","),
            (token::SEMICOLON, ";"),
            (token::EOF, ""),
        ];

        let mut lexer = new(input.to_string());

        for (i, case) in tests.iter().enumerate() {
            let tok = lexer.next_token();

            assert_eq!(
                &tok.type_, &case.0,
                "tests[{}] - tokentype wrong. expected={}, got={}",
                i, case.0, tok.type_
            );

            assert_eq!(
                &tok.literal, &case.1,
                "tests[{}] - literal wrong. expected={}, got={}",
                i, case.1, tok.literal
            );
        }
    }

    #[test]
    fn test_next_token2() {
        let input = r#"let five = 5;
      let ten = 10;

      let add = fn(x, y) {
          x + y
      };

      let result = add(five, ten);
      !-/*5;
      5 < 10 > 5;

      if (5 < 10) {
          return true;
      } else {
          return false;
      }

      10 == 10;
      10 != 9;

      "foobar"
      "foo bar"
      [1, 2];
      {"foo": "bar"};
      macro(x, y) { x + y };
      "#;

        let tests = vec![
            (token::LET, "let"),
            (token::IDENT, "five"),
            (token::ASSIGN, "="),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "ten"),
            (token::ASSIGN, "="),
            (token::INT, "10"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "add"),
            (token::ASSIGN, "="),
            (token::FUNCTION, "fn"),
            (token::LPAREN, "("),
            (token::IDENT, "x"),
            (token::COMMA, ","),
            (token::IDENT, "y"),
            (token::RPAREN, ")"),
            (token::LBRACE, "{"),
            (token::IDENT, "x"),
            (token::PLUS, "+"),
            (token::IDENT, "y"),
            (token::RBRACE, "}"),
            (token::SEMICOLON, ";"),
            (token::LET, "let"),
            (token::IDENT, "result"),
            (token::ASSIGN, "="),
            (token::IDENT, "add"),
            (token::LPAREN, "("),
            (token::IDENT, "five"),
            (token::COMMA, ","),
            (token::IDENT, "ten"),
            (token::RPAREN, ")"),
            (token::SEMICOLON, ";"),
            (token::BANG, "!"),
            (token::MINUS, "-"),
            (token::SLASH, "/"),
            (token::ASTERISK, "*"),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::INT, "5"),
            (token::LT, "<"),
            (token::INT, "10"),
            (token::GT, ">"),
            (token::INT, "5"),
            (token::SEMICOLON, ";"),
            (token::IF, "if"),
            (token::LPAREN, "("),
            (token::INT, "5"),
            (token::LT, "<"),
            (token::INT, "10"),
            (token::RPAREN, ")"),
            (token::LBRACE, "{"),
            (token::RETURN, "return"),
            (token::TRUE, "true"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            (token::ELSE, "else"),
            (token::LBRACE, "{"),
            (token::RETURN, "return"),
            (token::FALSE, "false"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            (token::INT, "10"),
            (token::EQ, "=="),
            (token::INT, "10"),
            (token::SEMICOLON, ";"),
            (token::INT, "10"),
            (token::NOTEQ, "!="),
            (token::INT, "9"),
            (token::SEMICOLON, ";"),
            (token::STRING, "foobar"),
            (token::STRING, "foo bar"),
            (token::LBRACKET, "["),
            (token::INT, "1"),
            (token::COMMA, ","),
            (token::INT, "2"),
            (token::RBRACKET, "]"),
            (token::SEMICOLON, ";"),
            (token::LBRACE, "{"),
            (token::STRING, "foo"),
            (token::COLON, ":"),
            (token::STRING, "bar"),
            (token::RBRACE, "}"),
            (token::SEMICOLON, ";"),
            (token::MACRO, "macro"),
            (token::LPAREN, "("),
            (token::IDENT, "x"),
            (token::COMMA, ","),
            (token::IDENT, "y"),
            (token::RPAREN, ")"),
            (token::LBRACE, "{"),
            (token::IDENT, "x"),
            (token::PLUS, "+"),
            (token::IDENT, "y"),
            (token::RBRACE, "}"),
            (token::SEMICOLON, ";"),
            (token::EOF, ""),
        ];

        let mut lexer = new(input.to_string());

        for (i, case) in tests.iter().enumerate() {
            let tok = lexer.next_token();

            assert_eq!(
                &tok.type_, &case.0,
                "tests[{}] - tokentype wrong. expected={}, got={}",
                i, case.0, tok.type_
            );

            assert_eq!(
                &tok.literal, &case.1,
                "tests[{}] - literal wrong. expected={}, got={}",
                i, case.1, tok.literal
            );
        }
    }
}
