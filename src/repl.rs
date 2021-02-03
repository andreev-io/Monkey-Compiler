use crate::repl::{
    compiler::Compiler, eval::Evaluator, lexer::Lexer, object::Object, parser::Parser, vm::VM,
};
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

    let mut constants: Vec<Object> = Vec::new();
    let mut globals = Vec::with_capacity(vm::MAX_GLOBALS);
    globals.resize_with(vm::MAX_GLOBALS, Default::default);
    let mut symbols = compiler::SymbolTable::new();

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

        let comp = Compiler::new_with_state(symbols, constants);
        let (bytecode, new_symbols) = comp.compile(program);
        symbols = new_symbols;
        println!("{:?}", bytecode);
        constants = bytecode.constants.clone();

        let mut machine = VM::new_with_existing_globals(bytecode, globals);
        machine.run();

        let popped = machine.get_last_popped();
        globals = machine.globals;

        writeln!(stdout, "{:?}", popped)?;
    }
}
