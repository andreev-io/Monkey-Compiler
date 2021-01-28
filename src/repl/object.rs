use crate::repl::lexer::Token;
use crate::repl::parser::Statement;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    String(String),
    Function(Vec<Token>, Box<Statement>),
    ReturnValue(Box<Object>),
    Array(Vec<Box<Object>>),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(i) => i.to_string(),
            Object::Boolean(b) => b.to_string(),
            Object::ReturnValue(rv) => rv.inspect(),
            Object::String(s) => s.to_string(),
            Object::Array(arr) => {
                let mut s: String = String::from("[");
                let mut v = Vec::new();

                for el in arr.into_iter() {
                    v.push(el.inspect());
                }

                s.push_str(&v.join(","));
                s.push_str("]");

                s
            }
            Object::Function(params, body) => {
                let mut s: String = String::from("");

                let mut v = Vec::new();
                for param in params {
                    v.push(param.string());
                }

                s.push_str("fn(");
                s.push_str(&v.join(", "));
                s.push_str(") ");
                s.push_str(&format!("{:?}", body));
                s.push_str("\n");

                s
            }
            Object::Null => String::from("null"),
        }
    }
}

pub struct Environment(HashMap<String, Object>);

impl Environment {
    pub fn new() -> Environment {
        Environment(HashMap::new())
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.0.insert(name, val);
    }

    pub fn get(&mut self, name: String) -> Option<&Object> {
        self.0.get(&name)
    }
}
