use crate::repl::code::{Instructions, OpCode, OP};
use crate::repl::compiler;
use crate::repl::object::Object;

pub const STACK_SIZE: usize = 2048;

pub const MAX_GLOBALS: usize = 65536;

const MAX_FRAMES: usize = 1024;

struct Frame {
    // closure
    clos: Object,
    // instruction pointer
    ip: usize,
    // base pointer
    bp: usize,
}

impl Frame {
    fn new(closure: Object, bp: usize) -> Frame {
        match closure {
            Object::CompiledClosure { .. } => {}
            _ => panic!("frame can only be constructed from a closure"),
        };

        Frame {
            clos: closure,
            ip: 0,
            bp,
        }
    }

    fn ins(&self) -> &Instructions {
        match &self.clos {
            Object::CompiledClosure {
                ins,
                num_locals: _,
                num_args: _,
                frees: _,
            } => ins,
            _ => panic!(),
        }
    }

    fn get_free(&self, i: usize) -> &Object {
        match &self.clos {
            Object::CompiledClosure {
                ins: _,
                num_locals: _,
                num_args: _,
                frees,
            } => return &frees[i],
            _ => panic!(),
        };
    }
}

pub struct VM {
    // stack of (call) frames
    frames: Vec<Frame>,

    last_popped: Object,
    constants: Vec<Object>,

    // vectors are so convenient we won't even have to maintain a stack pointer
    stack: Vec<Object>,

    pub globals: Vec<Object>,
}

impl VM {
    pub fn new(code: compiler::Bytecode) -> VM {
        let main_frame = Frame::new(
            Object::CompiledClosure {
                ins: code.instructions,
                num_locals: 0,
                num_args: 0,
                frees: Vec::new(),
            },
            0,
        );
        let mut frames = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);

        let mut globals = Vec::with_capacity(MAX_GLOBALS);
        globals.resize_with(MAX_GLOBALS, Default::default);
        let stack = Vec::with_capacity(STACK_SIZE);

        VM {
            constants: code.constants,
            stack: stack,
            last_popped: Object::Null,
            globals: globals,
            frames,
        }
    }

    fn current_frame(&mut self) -> &mut Frame {
        let last_idx = self.frames.len() - 1;
        self.frames.get_mut(last_idx).unwrap()
    }

    fn pop_frame(&mut self) -> Option<Frame> {
        self.frames.pop()
    }

    fn push_frame(&mut self, f: Frame) {
        self.frames.push(f);
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
        while self.current_frame().ip < self.current_frame().ins().len() {
            let ip = self.current_frame().ip;
            let op = self.current_frame().ins().get(ip);
            match self.current_frame().ins().get(ip) {
                OP::CLOS => {
                    let f_idx = u16::from_be_bytes([
                        self.current_frame().ins().get(ip + 1),
                        self.current_frame().ins().get(ip + 2),
                    ]) as usize;

                    let num_free =
                        u8::from_be_bytes([self.current_frame().ins().get(ip + 3)]) as usize;

                    self.current_frame().ip += 4;

                    let mut frees = Vec::new();
                    for i in 0..num_free {
                        frees.push(self.stack[self.stack.len() - 1 - i].clone());
                    }
                    let closure_proto = self.constants[f_idx].clone();
                    match closure_proto {
                        Object::CompiledClosure {
                            ins,
                            num_locals,
                            num_args,
                            ..
                        } => {
                            self.push(Object::CompiledClosure {
                                ins: ins,
                                num_locals: num_locals,
                                num_args: num_args,
                                frees: frees,
                            });
                        }
                        _ => {}
                    }
                }
                OP::GET_FREE => {
                    let idx = u8::from_be_bytes([self.current_frame().ins().get(ip + 1)]) as usize;
                    self.current_frame().ip += 2;
                    let free = self.current_frame().get_free(idx).clone();
                    self.push(free);
                }
                OP::GET_LOC => {
                    let idx = u8::from_be_bytes([self.current_frame().ins().get(ip + 1)]) as usize;
                    self.current_frame().ip += 2;

                    let bp = self.current_frame().bp;
                    self.push(self.stack[bp + idx].clone());
                }
                OP::SET_LOC => {
                    let idx = u8::from_be_bytes([self.current_frame().ins().get(ip + 1)]) as usize;
                    self.current_frame().ip += 2;
                    let local = self.pop();
                    let bp = self.current_frame().bp;
                    self.stack[bp + idx] = local;
                }
                OP::RET_NONE => {
                    // pop the call frame
                    let popped_bp = self.pop_frame().unwrap().bp;
                    // pop local bindings
                    while self.stack.len() >= popped_bp {
                        self.pop();
                    }

                    self.push(Object::Null);
                }
                OP::RET_VAL => {
                    // pop the return value
                    let ret_val = self.pop();
                    // pop the call frame
                    let popped_bp = self.pop_frame().unwrap().bp;
                    // pop local bindings
                    while self.stack.len() > popped_bp {
                        self.pop();
                    }

                    // push the return value
                    self.push(ret_val);
                }
                OP::CALL => {
                    let num_passed_args =
                        u8::from_be_bytes([self.current_frame().ins().get(ip + 1)]) as usize;
                    let f = self.stack.remove(self.stack.len() - 1 - num_passed_args);
                    // important to advance current frame before pushing a new
                    // frame
                    self.current_frame().ip += 2;
                    let (num_args, num_locals) = match f {
                        Object::CompiledClosure {
                            ins: _,
                            num_locals,
                            num_args,
                            ..
                        } => (num_args.clone(), num_locals.clone()),
                        _ => panic!(),
                    };

                    if num_passed_args != num_args {
                        panic!("got {} params expected {}", num_passed_args, num_args);
                    }

                    self.push_frame(Frame::new(f, self.stack.len() - num_args));
                    // Allocate space for local bindings on the stack.
                    for _ in 0..num_locals {
                        self.push(Object::Null);
                    }
                }
                OP::IDX => {
                    let index = self.pop();
                    let arr = self.pop();

                    match index {
                        Object::Integer(i) => {
                            let o = arr[i as usize].clone();
                            self.push(o);
                        }
                        _ => {}
                    };

                    self.current_frame().ip += 1;
                }
                OP::ARR => {
                    let len = u16::from_be_bytes([
                        self.current_frame().ins().get(ip + 1),
                        self.current_frame().ins().get(ip + 2),
                    ]) as usize;
                    self.current_frame().ip += 3;

                    let mut arr_proto = Vec::new();
                    arr_proto.resize(len, Box::new(Object::Null));
                    for i in (0..len).rev() {
                        arr_proto[i] = Box::new(self.pop());
                    }

                    let array = Object::Array(arr_proto);
                    self.push(array);
                }
                OP::GET_GLOB => {
                    //println!("getting a global");
                    let index = u16::from_be_bytes([
                        self.current_frame().ins().get(ip + 1),
                        self.current_frame().ins().get(ip + 2),
                    ]) as usize;
                    self.current_frame().ip += 3;
                    self.push(self.globals[index].clone());
                }
                OP::SET_GLOB => {
                    let index = u16::from_be_bytes([
                        self.current_frame().ins().get(ip + 1),
                        self.current_frame().ins().get(ip + 2),
                    ]) as usize;
                    self.current_frame().ip += 3;
                    self.globals[index] = self.pop();
                }
                OP::SET_NULL => {
                    self.stack.push(Object::Null);
                    self.current_frame().ip += 1;
                }
                OP::JMP => {
                    // TODO: make function to get operands
                    self.current_frame().ip = u16::from_be_bytes([
                        self.current_frame().ins().get(ip + 1),
                        self.current_frame().ins().get(ip + 2),
                    ]) as usize;
                }
                OP::JMP_IF_NOT => {
                    let pos = u16::from_be_bytes([
                        self.current_frame().ins().get(ip + 1),
                        self.current_frame().ins().get(ip + 2),
                    ]) as usize;
                    self.current_frame().ip += 3;

                    let cond = self.pop();
                    match !cond {
                        Object::Boolean(true) => self.current_frame().ip = pos,
                        _ => {}
                    }
                }
                OP::POP => {
                    self.last_popped = self.pop();
                    self.current_frame().ip += 1;
                }
                OP::CONSTANT => {
                    let const_index = u16::from_be_bytes([
                        self.current_frame().ins().get(ip + 1),
                        self.current_frame().ins().get(ip + 2),
                    ]);

                    // take the constant out of the constants pool and push it
                    // onto the VM stack
                    self.push(self.constants[const_index as usize].clone());

                    self.current_frame().ip += 3;
                }
                OP::TRUE | OP::FALSE => {
                    if op == OP::TRUE {
                        self.push(Object::Boolean(true))
                    } else {
                        self.push(Object::Boolean(false))
                    }

                    self.current_frame().ip += 1;
                }
                OP::GT | OP::EQ | OP::NE => {
                    self.exec_comparison_op(op);
                    self.current_frame().ip += 1;
                }
                OP::NOT | OP::NEG => {
                    let operand = self.pop();
                    if op == OP::NEG {
                        self.push(-operand)
                    } else {
                        self.push(!operand)
                    }

                    self.current_frame().ip += 1;
                }
                OP::ADD | OP::DIV | OP::MUL | OP::SUB => {
                    self.exec_binary_op(op);
                    self.current_frame().ip += 1;
                }
                _ => self.current_frame().ip += 1,
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
