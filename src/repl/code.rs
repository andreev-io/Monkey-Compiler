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

// Points at data in the constants pool. Single operand 2 bytes wide.
pub const OP_CONSTANT: OpCode = 0;

pub const OP_ADD: OpCode = 1;

struct Definition {
    name: String,
    operands_width: Vec<usize>,
}

lazy_static! {
    static ref DEFINITIONS: HashMap<OpCode, Definition> = {
        let mut m = HashMap::new();
        m.insert(
            OP_CONSTANT,
            Definition {
                name: String::from("OpConstant"),
                operands_width: vec![2],
            },
        );

        m.insert(
            OP_ADD,
            Definition {
                name: String::from("OpConstant"),
                operands_width: vec![],
            },
        );

        m
    };
}

pub fn make(op: OpCode, operands: &[i32]) -> Instructions {
    let def = match DEFINITIONS.get(&op) {
        Some(d) => d,
        None => return Instructions(Vec::new()),
    };

    let mut instruction: Vec<u8> = vec![0; def.operands_width.iter().fold(1, |acc, len| acc + len)];
    instruction[0] = op;

    let mut offset = 1;
    for (i, operand) in operands.iter().enumerate() {
        let width = def.operands_width[i];
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
                op: code::OP_CONSTANT,
                operands: &[65534],
                expected: code::Instructions(vec![code::OP_CONSTANT, 0xFF, 0xFE]),
            },
            T {
                op: code::OP_ADD,
                operands: &[],
                expected: code::Instructions(vec![code::OP_ADD]),
            },
        ];

        for test in tests {
            let instruction = code::make(test.op, test.operands);
            assert_eq!(instruction.0.len(), test.expected.0.len());
            assert_eq!(instruction, test.expected);
        }
    }
}
