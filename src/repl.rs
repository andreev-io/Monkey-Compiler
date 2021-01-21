use crate::repl::{
    lexer::Lexer,
    object::{Environment, Node},
    parser::Parser,
};
use std::io::{Read, Write};

mod eval;
mod lexer;
mod object;
mod parser;

pub fn run_repl(stdin: &mut dyn Read, stdout: &mut dyn Write) -> Result<(), std::io::Error> {
    writeln!(stdout, "Welcome to the Monkey REPL!")?;
    loop {
        let mut buffer = String::new();
        write!(stdout, "\n>>> ")?;

        std::io::stdout().flush().unwrap();
        stdin.read_to_string(&mut buffer).unwrap();
        writeln!(stdout)?;

        let chars = buffer.chars().collect();
        let lex = Lexer::new(&chars);
        let mut parser = Parser::new(lex);
        let mut env = Box::new(Environment::new());
        let program = parser.parse_program();

        writeln!(stdout, "{}", program.eval(&mut env).inspect())?;
    }
}
