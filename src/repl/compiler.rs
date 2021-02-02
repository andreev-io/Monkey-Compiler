use crate::repl::code;
use crate::repl::code::{Instructions, OpCode};
use crate::repl::lexer::{TokenType, TokenValue};
use crate::repl::object::Object;
use crate::repl::parser::{Expression, Program, Statement};

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            instructions: Instructions(Vec::new()),
            constants: Vec::new(),
        }
    }

    pub fn compile(mut self, p: Program) -> Bytecode {
        for statement in p.statements {
            self.compile_statement(*statement);
        }

        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }

    fn compile_statement(&mut self, s: Statement) {
        match s {
            Statement::Expression(exp) => self.compile_expression(*exp),
            _ => {}
        }
    }

    fn compile_expression(&mut self, e: Expression) {
        match e {
            Expression::Infix(left, token, right) => {
                // We're compiling for a stack-based virtual machine. That means
                // that an expression like 1 + 2 is ordered like 1|2|+ on the stack.
                self.compile_expression(*left);
                self.compile_expression(*right);

                match token.t_type {
                    TokenType::Plus => {
                        self.emit_instruction(code::OP_ADD, &[]);
                    }
                    _ => {}
                }
            }
            Expression::Integer(token) => {
                let obj = match token.t_value {
                    Some(TokenValue::Numeric(int)) => Object::Integer(int),
                    _ => Object::Null,
                };

                // Place the constant in the pool and emit instruction with the
                // address as the operand.
                let address = [self.add_constant(obj) as i32];
                self.emit_instruction(code::OP_CONSTANT, &address);
            }
            _ => {}
        }
    }

    // Method to add a constant to the constants pool
    fn add_constant(&mut self, o: Object) -> usize {
        self.constants.push(o);
        self.constants.len() - 1
    }

    // Returns the starting point of the added instruction
    fn emit_instruction(&mut self, op: OpCode, operands: &[i32]) -> usize {
        let ins = code::make(op, operands);
        let pos = self.add_instruction(ins);
        pos
    }

    // Returns the starting point of the added instruction
    fn add_instruction(&mut self, instructions: Instructions) -> usize {
        let pos = self.instructions.0.len();
        self.instructions.push_instructions(instructions);

        pos
    }
}
