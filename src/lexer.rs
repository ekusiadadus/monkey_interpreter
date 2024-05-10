use crate::token::{Token, TokenKind};

struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: Default::default(),
        };

        lexer.read_char();

        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        let token = match self.ch {
            '=' => self.new_token(TokenKind::Assign, self.ch),
            ';' => self.new_token(TokenKind::Semicolon, self.ch),
            '(' => self.new_token(TokenKind::LParen, self.ch),
            ')' => self.new_token(TokenKind::RParen, self.ch),
            ',' => self.new_token(TokenKind::Comma, self.ch),
            '+' => self.new_token(TokenKind::Plus, self.ch),
            '{' => self.new_token(TokenKind::LBrace, self.ch),
            '}' => self.new_token(TokenKind::RBrace, self.ch),
            '\0' => Token {
                kind: TokenKind::Eof,
                literal: "".to_string(),
            },
            _ => self.new_token(TokenKind::Illegal, self.ch),
        };

        self.read_char();

        token
    }

    fn new_token(&self, kind: TokenKind, ch: char) -> Token {
        Token {
            kind,
            literal: ch.to_string(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::{Token, TokenKind};

    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";

        let expected: Vec<Token> = vec![
            Token {
                kind: TokenKind::Assign,
                literal: "=".to_string(),
            },
            Token {
                kind: TokenKind::Plus,
                literal: "+".to_string(),
            },
            Token {
                kind: TokenKind::LParen,
                literal: "(".to_string(),
            },
            Token {
                kind: TokenKind::RParen,
                literal: ")".to_string(),
            },
            Token {
                kind: TokenKind::LBrace,
                literal: "{".to_string(),
            },
            Token {
                kind: TokenKind::RBrace,
                literal: "}".to_string(),
            },
            Token {
                kind: TokenKind::Comma,
                literal: ",".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Eof,
                literal: "".to_string(),
            },
        ];

        let mut lexer = Lexer::new(input);

        for (idx, exp_token) in expected.into_iter().enumerate() {
            let recv_token = &lexer.next_token();
            assert_eq!(
                exp_token.kind, recv_token.kind,
                "tests[{idx}] - token type wrong. expected={:?}, got={:?}",
                exp_token.kind, recv_token.kind
            );
            assert_eq!(
                exp_token.literal, recv_token.literal,
                "tests[{idx}] - token literal wrong. expected={}, got={}",
                exp_token.literal, recv_token.literal
            );
        }
    }

    #[test]
    fn test_next_token_1() {
        let input = r#"
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x+y;
        };

        let result = add(five,ten);
        "#;

        let expected: Vec<Token> = vec![
            Token {
                kind: TokenKind::Let,
                literal: "let".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "five".to_string(),
            },
            Token {
                kind: TokenKind::Assign,
                literal: "=".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "5".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Let,
                literal: "let".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "ten".to_string(),
            },
            Token {
                kind: TokenKind::Assign,
                literal: "=".to_string(),
            },
            Token {
                kind: TokenKind::Int,
                literal: "10".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Let,
                literal: "let".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "add".to_string(),
            },
            Token {
                kind: TokenKind::Assign,
                literal: "=".to_string(),
            },
            Token {
                kind: TokenKind::Function,
                literal: "fn".to_string(),
            },
            Token {
                kind: TokenKind::LParen,
                literal: "(".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "x".to_string(),
            },
            Token {
                kind: TokenKind::Comma,
                literal: ",".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "y".to_string(),
            },
            Token {
                kind: TokenKind::RParen,
                literal: ")".to_string(),
            },
            Token {
                kind: TokenKind::LBrace,
                literal: "{".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "x".to_string(),
            },
            Token {
                kind: TokenKind::Plus,
                literal: "+".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "y".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::RBrace,
                literal: "}".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Let,
                literal: "let".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "result".to_string(),
            },
            Token {
                kind: TokenKind::Assign,
                literal: "=".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "add".to_string(),
            },
            Token {
                kind: TokenKind::LParen,
                literal: "(".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "five".to_string(),
            },
            Token {
                kind: TokenKind::Comma,
                literal: ",".to_string(),
            },
            Token {
                kind: TokenKind::Ident,
                literal: "ten".to_string(),
            },
            Token {
                kind: TokenKind::RParen,
                literal: ")".to_string(),
            },
            Token {
                kind: TokenKind::Semicolon,
                literal: ";".to_string(),
            },
            Token {
                kind: TokenKind::Eof,
                literal: "".to_string(),
            },
        ];
        let mut lexer = Lexer::new(input);

        for (idx, exp_token) in expected.into_iter().enumerate() {
            let recv_token = &lexer.next_token();
            assert_eq!(
                exp_token.kind, recv_token.kind,
                "tests[{idx}] - token type wrong. expected={:?}, got={:?}",
                exp_token.kind, recv_token.kind
            );
            assert_eq!(
                exp_token.literal, recv_token.literal,
                "tests[{idx}] - token literal wrong. expected={}, got={}",
                exp_token.literal, recv_token.literal
            );
        }
    }
}
