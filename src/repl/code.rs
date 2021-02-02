use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub struct Instructions(pub Vec<u8>);

pub type OpCode = u8;

impl Instructions {
    pub fn push_instructions(&mut self, instructions: Instructions) {
        for instruction in instructions.0 {
            self.0.push(instruction);
        }
    }
}

#[non_exhaustive]
pub struct OP;

impl OP {
    pub const CONSTANT: OpCode = 0;
    pub const POP: OpCode = 1;
    pub const ADD: OpCode = 2;
    pub const SUB: OpCode = 3;
    pub const DIV: OpCode = 4;
    pub const MUL: OpCode = 5;
    pub const TRUE: OpCode = 6;
    pub const FALSE: OpCode = 7;
    pub const EQ: OpCode = 8;
    pub const NE: OpCode = 9;
    pub const GT: OpCode = 10;
    pub const NEG: OpCode = 11;
    pub const NOT: OpCode = 12;
}

struct Definition(Vec<usize>);

lazy_static! {
    static ref DEFINITIONS: HashMap<OpCode, Definition> = {
        let mut m = HashMap::new();
        m.insert(OP::CONSTANT, Definition(vec![2]));

        m.insert(OP::ADD, Definition(vec![]));

        m.insert(OP::SUB, Definition(vec![]));

        m.insert(OP::DIV, Definition(vec![]));

        m.insert(OP::MUL, Definition(vec![]));

        m.insert(OP::POP, Definition(vec![]));

        m.insert(OP::TRUE, Definition(vec![]));

        m.insert(OP::FALSE, Definition(vec![]));

        m.insert(OP::EQ, Definition(vec![]));

        m.insert(OP::NE, Definition(vec![]));

        m.insert(OP::GT, Definition(vec![]));

        m.insert(OP::NEG, Definition(vec![]));

        m.insert(OP::NOT, Definition(vec![]));

        m
    };
}

pub fn make(op: OpCode, operands: &[i32]) -> Instructions {
    let def = match DEFINITIONS.get(&op) {
        Some(d) => d,
        None => return Instructions(Vec::new()),
    };

    let mut instruction: Vec<u8> = vec![0; def.0.iter().fold(1, |acc, len| acc + len)];
    instruction[0] = op;

    let mut offset = 1;
    for (i, operand) in operands.iter().enumerate() {
        let width = def.0[i];
        let bytes = operand.to_be_bytes();
        let mut bytes_iter = bytes.iter();
        if bytes.len() > width {
            bytes_iter.nth(bytes.len() - width - 1);
        }

        for byte in bytes_iter {
            instruction[offset] = *byte;
            offset += 1;
        }
    }

    Instructions(instruction)
}

#[cfg(test)]
mod test {
    use crate::repl::code;

    #[test]
    fn test_make() {
        struct T<'a> {
            op: code::OpCode,
            operands: &'a [i32],
            expected: code::Instructions,
        }

        let tests = vec![
            T {
                op: code::OP::CONSTANT,
                operands: &[65534],
                expected: code::Instructions(vec![code::OP::CONSTANT, 0xFF, 0xFE]),
            },
            T {
                op: code::OP::ADD,
                operands: &[],
                expected: code::Instructions(vec![code::OP::ADD]),
            },
        ];

        for test in tests {
            let instruction = code::make(test.op, test.operands);
            assert_eq!(instruction.0.len(), test.expected.0.len());
            assert_eq!(instruction, test.expected);
        }
    }
}
