pub mod lexer;
pub mod token;
fn main() {
    let token = token::Token {
        kind: token::TokenKind::Ident,
        literal: "foobar".to_string(),
    };
    println!("{:?}", token)
}
