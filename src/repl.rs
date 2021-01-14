use crate::repl::lexer::{Lexer, Token};
use std::io::{Read, Write};

mod lexer;
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
        let mut lex = Lexer::new(&chars);
        let mut token = lex.next_token();
        while token != Token::EOF {
            writeln!(stdout, "{:?}", token)?;
            token = lex.next_token();
        }
    }
}
