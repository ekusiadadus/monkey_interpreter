use std::io;

use crate::repl::start;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod token;
fn main() {
    println!("Hello! This is the Monkey Programming language!");
    println!("Feel free to type in commands");
    start(io::stdin(), io::stdout());

    println!("Goodbye!");
}
