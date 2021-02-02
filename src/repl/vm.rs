use crate::repl::code;
use crate::repl::code::{Instructions, OpCode};
use crate::repl::compiler;
use crate::repl::object::Object;

const STACK_SIZE: usize = 2048;

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,

    // vectors are so convenient we won't even have to maintain a stack pointer
    stack: Vec<Object>,
}

impl VM {
    pub fn new(code: compiler::Bytecode) -> VM {
        VM {
            constants: code.constants,
            instructions: code.instructions,
            stack: Vec::with_capacity(STACK_SIZE),
        }
    }

    pub fn run(&mut self) {
        let mut ip = 0;
        while ip < self.instructions.0.len() {
            // fetch
            let op = self.instructions.0[ip] as OpCode;
            // decode
            match op {
                code::OP_CONSTANT => {
                    let const_index = u16::from_be_bytes([
                        self.instructions.0[ip + 1],
                        self.instructions.0[ip + 2],
                    ]);

                    // take the constant out of the constants pool and push it
                    // onto the VM stack
                    self.push(self.constants[const_index as usize].clone());

                    ip += 3;
                }
                code::OP_ADD => {
                    let right = self.pop();
                    let left = self.pop();

                    self.push(left + right);
                    ip += 1;
                }
                _ => ip += 1,
            }
        }
    }

    pub fn get_stack(&mut self) -> Vec<Object> {
        self.stack.clone()
    }

    fn push(&mut self, obj: Object) {
        if self.stack.len() >= STACK_SIZE {
            panic!("stack overflow");
        }

        self.stack.push(obj);
    }

    fn pop(&mut self) -> Object {
        if self.stack.len() == 0 {
            panic!("popping empty stack");
        }

        self.stack.pop().unwrap()
    }
}
