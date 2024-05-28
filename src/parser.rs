use std::collections::HashMap;

use crate::{
    ast::{ExpressionNode, Identifier, LetStatement, Program, ReturnStatement, StatementNode},
    lexer::Lexer,
    token::{Token, TokenKind},
};

type PrefixParseFn = fn(parser: &mut Parser) -> Option<ExpressionNode>;
type InfixParseFn = fn(parser: &mut Parser, exp: ExpressionNode) -> Option<ExpressionNode>;

struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenKind, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenKind, InfixParseFn>,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            cur_token: Default::default(),
            peek_token: Default::default(),
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
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
        let mut program: Program = Program { statements: vec![] };

        while !self.cur_token_is(TokenKind::Eof) {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }

        Some(program)
    }

    pub fn parse_statement(&mut self) -> Option<StatementNode> {
        match self.cur_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => None,
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<StatementNode> {
        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: Default::default(),
            value: Default::default(),
        };

        return if !self.expect_peek(TokenKind::Ident) {
            None
        } else {
            stmt.name = Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };

            if !self.expect_peek(TokenKind::Assign) {
                None
            } else {
                self.next_token();
                while !self.cur_token_is(TokenKind::Semicolon) {
                    self.next_token();
                }

                Some(StatementNode::Let(stmt))
            }
        };
    }

    pub fn parse_return_statement(&mut self) -> Option<StatementNode> {
        let stmt = ReturnStatement {
            token: self.cur_token.clone(),
            ret_value: Default::default(),
        };

        self.next_token();

        while !self.cur_token_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Some(StatementNode::Return(stmt))
    }

    fn expect_peek(&mut self, token_kind: TokenKind) -> bool {
        if self.peek_token_is(token_kind.clone()) {
            self.next_token();
            return true;
        }
        self.peek_error(token_kind);
        false
    }

    fn peek_token_is(&self, token_kind: TokenKind) -> bool {
        self.peek_token.kind == token_kind
    }

    fn cur_token_is(&self, token_kind: TokenKind) -> bool {
        self.cur_token.kind == token_kind
    }

    fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, token_kind: TokenKind) {
        let msg: String = format!(
            "expected next token to be {}, got {} insted",
            token_kind, self.peek_token.kind
        );

        self.errors.push(msg);
    }

    fn register_prefix(&mut self, token_kind: TokenKind, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_kind, func);
    }

    fn register_infix(&mut self, token_kind: TokenKind, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_kind, func);
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

        check_parser_errors(parser);

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

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
        "#;

        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parser_errors(parser);

        match program {
            Some(program) => {
                assert_eq!(
                    program.statements.len(),
                    3,
                    "statements does not contain 3 statements. got = {}",
                    program.statements.len()
                );

                for stmt in program.statements {
                    match stmt {
                        StatementNode::Return(ret_stmt) => {
                            assert_eq!(
                                ret_stmt.token_literal(),
                                "return",
                                "token literal not `return`, got={}",
                                ret_stmt.token_literal()
                            );
                        }
                        other => panic!("not Return Statement. got={:?}", other),
                    }
                }
            }
            None => panic!("parse program shoud not be none"),
        }
    }

    fn test_let_statement(statement: &StatementNode, expected: &str) {
        assert_eq!(
            statement.token_literal(),
            "let",
            "statement.token_literal not 'let'. got={}",
            statement.token_literal()
        );

        match statement {
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
            _ => panic!("not a Let Statement"),
        }
    }

    fn check_parser_errors(parser: Parser) {
        let errors = parser.errors();

        if errors.len() == 0 {
            return;
        }

        for error in errors {
            eprintln!("parser error: {}", error);
        }

        panic!("parser error parent");
    }
}
