use crate::{ast::{Identifier, LetStatement, Program, StatementNode}, lexer::Lexer, token::{Token, TokenKind}};

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            cur_token: Default::default(),
            peek_token: Default::default(),
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program: Program = Program {
            statements: vec![],
        };

        while self.cur_token.kind != TokenKind::Eof {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        };

        Some(program)
    }

    pub fn parse_statement(&mut self) -> Option<StatementNode> {
        match self.cur_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            _ => None
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<StatementNode> {
        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: Default::default(),
            value: Default::default(),
        };

        return if self.expect_peek(TokenKind::Ident) {
            None
        } else {
            stmt.name = Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone()
            };

            if !self.expect_peek(TokenKind::Assign){
                None
            } else {
                self.next_token();
                while !self.cur_token_is(TokenKind::Semicolon) {
                    self.next_token();
                }

                Some(StatementNode::Let(stmt))
            }
        }
    }

    fn expect_peek(&mut self, token_kind: TokenKind) -> bool {
        if self.peek_token_is(token_kind) {
            self.next_token();
            return true;
        } 
        false
    }

    fn peek_token_is(&self, token_kind: TokenKind) -> bool {
        self.peek_token.kind == token_kind
    }

    fn cur_token_is(&self, token_kind: TokenKind) -> bool {
        self.cur_token.kind == token_kind
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::{
        ast::{Node, StatementNode},
        lexer::Lexer,
    };

    #[test]
    fn test_let_statements() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        match program {
            Some(program) => {
                assert_eq!(
                    program.statements.len(),
                    3,
                    "statements does not contain 3 statements, got={}",
                    program.statements.len()
                );

                let expected = vec!["x", "y", "foobar"];

                for (idx, exp) in expected.iter().enumerate() {
                    let statement = &program.statements[idx];

                    test_let_statement(statement, exp);
                }
            }
            None => {
                panic!("parse_program() returned None");
            }
        }
    }

    fn test_let_statement(statement: &StatementNode, expected: &str) {
        assert_eq!(
            statement.token_literal(),
            "let",
            "statement.token_literal not 'let'. got={}",
            statement.token_literal()
        );

        match &statement {
            StatementNode::Let(let_statement) => {
                assert_eq!(
                    let_statement.name.value, expected,
                    "let_statement.name.value not '{}'. got={}",
                    expected, let_statement.name.value
                );

                assert_eq!(
                    let_statement.name.token_literal(),
                    expected,
                    "let_statement.name.token_literal not '{}'. got={}",
                    expected,
                    let_statement.name.token_literal()
                );
            }
        }
    }
}
