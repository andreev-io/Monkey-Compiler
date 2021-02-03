use crate::repl::code::{Instructions, OpCode, OP};
use crate::repl::compiler;
use crate::repl::object::Object;

pub const STACK_SIZE: usize = 2048;

pub const MAX_GLOBALS: usize = 65536;

pub struct VM {
    last_popped: Object,
    constants: Vec<Object>,
    instructions: Instructions,

    // vectors are so convenient we won't even have to maintain a stack pointer
    stack: Vec<Object>,

    pub globals: Vec<Object>,
}

impl VM {
    pub fn new(code: compiler::Bytecode) -> VM {
        let mut globals = Vec::with_capacity(MAX_GLOBALS);
        globals.resize_with(MAX_GLOBALS, Default::default);
        let stack = Vec::with_capacity(STACK_SIZE);

        VM {
            constants: code.constants,
            instructions: code.instructions,
            stack: stack,
            last_popped: Object::Null,
            globals: globals,
        }
    }

    pub fn new_with_existing_globals(code: compiler::Bytecode, globals: Vec<Object>) -> VM {
        let mut vm = VM::new(code);
        vm.globals = globals;
        vm
    }

    pub fn get_last_popped(&self) -> Object {
        self.last_popped.clone()
    }

    pub fn run(&mut self) {
        let mut ip = 0;
        while ip < self.instructions.0.len() {
            // fetch
            let op = self.instructions.0[ip] as OpCode;
            // decode
            match op {
                OP::GET_GLOB => {
                    let index = u16::from_be_bytes([
                        self.instructions.0[ip + 1],
                        self.instructions.0[ip + 2],
                    ]) as usize;

                    ip += 3;

                    self.push(self.globals[index].clone());
                }
                OP::SET_GLOB => {
                    let index = u16::from_be_bytes([
                        self.instructions.0[ip + 1],
                        self.instructions.0[ip + 2],
                    ]) as usize;

                    ip += 3;

                    self.globals[index] = self.pop();
                }
                OP::SET_NULL => {
                    self.stack.push(Object::Null);
                    ip += 1;
                }
                OP::JMP => {
                    // TODO: make function to get operands
                    ip = u16::from_be_bytes([
                        self.instructions.0[ip + 1],
                        self.instructions.0[ip + 2],
                    ]) as usize;
                }
                OP::JMP_IF_NOT => {
                    let pos = u16::from_be_bytes([
                        self.instructions.0[ip + 1],
                        self.instructions.0[ip + 2],
                    ]) as usize;
                    ip += 3;

                    let cond = self.pop();
                    match !cond {
                        Object::Boolean(true) => ip = pos,
                        _ => {}
                    }
                }
                OP::POP => {
                    self.last_popped = self.pop();
                    ip += 1;
                }
                OP::CONSTANT => {
                    let const_index = u16::from_be_bytes([
                        self.instructions.0[ip + 1],
                        self.instructions.0[ip + 2],
                    ]);

                    // take the constant out of the constants pool and push it
                    // onto the VM stack
                    self.push(self.constants[const_index as usize].clone());

                    ip += 3;
                }
                OP::TRUE | OP::FALSE => {
                    if op == OP::TRUE {
                        self.push(Object::Boolean(true))
                    } else {
                        self.push(Object::Boolean(false))
                    }

                    ip += 1;
                }
                OP::GT | OP::EQ | OP::NE => {
                    self.exec_comparison_op(op);
                    ip += 1;
                }
                OP::NOT | OP::NEG => {
                    let operand = self.pop();
                    if op == OP::NEG {
                        self.push(-operand)
                    } else {
                        self.push(!operand)
                    }

                    ip += 1;
                }
                OP::ADD | OP::DIV | OP::MUL | OP::SUB => {
                    self.exec_binary_op(op);
                    ip += 1;
                }
                _ => ip += 1,
            }
        }
    }

    fn exec_comparison_op(&mut self, op: OpCode) {
        let left = self.pop();
        let right = self.pop();

        match op {
            OP::GT => self.push(Object::Boolean(left > right)),
            OP::NE => self.push(Object::Boolean(left != right)),
            OP::EQ => self.push(Object::Boolean(left == right)),
            _ => {}
        }
    }

    fn exec_binary_op(&mut self, op: OpCode) {
        match op {
            OP::ADD => {
                let right = self.pop();
                let left = self.pop();

                self.push(left + right);
            }
            OP::MUL => {
                let right = self.pop();
                let left = self.pop();

                self.push(left * right);
            }
            OP::SUB => {
                let right = self.pop();
                let left = self.pop();

                self.push(left - right);
            }
            OP::DIV => {
                let right = self.pop();
                let left = self.pop();

                self.push(left / right);
            }
            _ => {}
        }
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
