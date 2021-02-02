use crate::repl::{eval::Evaluator, lexer::Lexer, parser::Parser, compiler::Compiler, vm::VM};
use std::io::{Read, Write};

mod code;
mod compiler;
mod eval;
mod lexer;
mod object;
mod parser;
mod vm;

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
        let program = parser.parse_program();

        let mut comp = Compiler::new();
        let bytecode = comp.compile(program);

        let mut machine = VM::new(bytecode);
        machine.run();

        let stack = machine.get_stack();

        writeln!(stdout, "{:?}", stack)?;
    }
}
