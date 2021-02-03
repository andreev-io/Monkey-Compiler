use crate::repl::lexer::Token;
use crate::repl::parser::Statement;
use core::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    String(String),
    Function(Vec<Token>, Box<Statement>, Rc<RefCell<Environment>>),
    ReturnValue(Box<Object>),
    Array(Vec<Box<Object>>),
    Null,
}

impl Default for Object {
    fn default() -> Object {
        Object::Null
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => left == right,
            (Object::Boolean(left), Object::Boolean(right)) => left == right,
            (Object::String(left), Object::String(right)) => left == right,
            (_, _) => false,
        }
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => {
                if left > right {
                    Some(Ordering::Less)
                } else if left == right {
                    Some(Ordering::Equal)
                } else {
                    Some(Ordering::Greater)
                }
            }
            (_, _) => None,
        }
    }
}

impl Neg for Object {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            Object::Integer(i) => Object::Integer(-i),
            _ => Object::Null,
        }
    }
}

impl Not for Object {
    type Output = Self;

    fn not(self) -> Self {
        match self {
            Object::Boolean(b) => Object::Boolean(!b),
            Object::Null => Object::Boolean(true),
            _ => Object::Boolean(false),
        }
    }
}

impl Mul for Object {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left * right),
            (_, _) => Object::Null,
        }
    }
}

impl Div for Object {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left / right),
            (_, _) => Object::Null,
        }
    }
}

impl Add for Object {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left + right),
            (Object::String(left), Object::String(right)) => {
                Object::String(format!("{}{}", left, right))
            }
            (_, _) => Object::Null,
        }
    }
}

impl Sub for Object {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left - right),
            (_, _) => Object::Null,
        }
    }
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
            Object::Function(params, body, _) => {
                let mut s: String = String::from("");

                let mut v = Vec::new();
                for param in params {
                    v.push(param.string());
                }

                s.push_str("fn(");
                s.push_str(&v.join(", "));
                s.push_str(") ");
                s.push_str(&body.string());
                s.push_str("\n");

                s
            }
            Object::Null => String::from("null"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Environment(HashMap<String, Object>, Option<Rc<RefCell<Environment>>>);

impl Environment {
    pub fn new() -> Environment {
        Environment(HashMap::new(), None)
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Environment {
        Environment(HashMap::new(), Some(outer))
    }

    pub fn set(&mut self, name: String, val: Object) {
        self.0.insert(name, val);
    }

    pub fn get(&mut self, name: String) -> Option<Object> {
        if let Some(obj) = self.0.get(&name) {
            Some(obj.clone())
        } else {
            if let Some(outer) = &self.1 {
                outer.borrow_mut().get(name).clone()
            } else {
                None
            }
        }
    }
}
