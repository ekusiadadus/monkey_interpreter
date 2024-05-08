pub struct Lexer {
    pub input: Vec<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::{Token, TokenKind};

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

        let lexer = Lexer::new(input);

        for token in expected.into_iter().enumerate() {
            let tok = lexer.next_token();
            assert_eq!(tok.kind, token.1.kind);
            assert_eq!(tok.literal, token.1.literal);
        }
    }
}
