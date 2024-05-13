use crate::{ast::Program, lexer::Lexer, token::Token};

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
        None
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
            other => {
                panic!("statement not LetStatement. got={:?}", other);
            }
        }
    }
}
