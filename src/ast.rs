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
