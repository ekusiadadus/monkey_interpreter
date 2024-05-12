use crate::token::Token;

trait Node {
    fn token_literal(&self) -> String;
    fn print_string(&self) -> String;
}

enum StatementNode {
    Let,
}

impl Node for StatementNode {
    fn token_literal(&self) -> String {
        match self {
            StatementNode::Let => "let".to_string(),
        }
    }

    fn print_string(&self) -> String {
        match self {
            StatementNode::Let => "let".to_string(),
        }
    }
}

enum ExpressionNode {
    Identifier,
}

impl Node for ExpressionNode {
    fn token_literal(&self) -> String {
        match self {
            ExpressionNode::Identifier => "identifier".to_string(),
        }
    }

    fn print_string(&self) -> String {
        match self {
            ExpressionNode::Identifier => "identifier".to_string(),
        }
    }
}

struct Program {
    statements: Vec<StatementNode>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }

    fn print_string(&self) -> String {
        let mut out = String::new();
        for statement in &self.statements {
            out.push_str(&statement.print_string());
        }
        out
    }
}

struct LetStatement {
    token: Token,
    name: Identifier,
    value: Option<ExpressionNode>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        let mut out = String::new();
        out.push_str(&self.token_literal().as_str());
        out.push_str(" ");
        out.push_str(&self.name.print_string().as_str());
        out.push_str(" = ");

        if let Some(value) = &self.value {
            out.push_str(&value.print_string().as_str());
        }

        out.push_str(";");

        out
    }
}

struct Identifier {
    token: Token,
    value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print_string(&self) -> String {
        self.value.clone()
    }
}
