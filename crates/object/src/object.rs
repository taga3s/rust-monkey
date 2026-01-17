//! Object of Evaluation for the Monkey interpreter.

use core::str;
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use ast::ast::{BlockStatement, Identifier};

use crate::environment::Environment;

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum ObjectType {
    IntegerObj,
    StringObj,
    BooleanObj,
    ArrayObj,
    NullObj,
    ReturnValueObj,
    FunctionObj,
    BuiltinObj,
    HashObj,
    ErrorObj,
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let type_str = match self {
            ObjectType::IntegerObj => "INTEGER",
            ObjectType::StringObj => "STRING",
            ObjectType::BooleanObj => "BOOLEAN",
            ObjectType::ArrayObj => "ARRAY",
            ObjectType::NullObj => "NULL",
            ObjectType::ReturnValueObj => "RETURN_VALUE",
            ObjectType::FunctionObj => "FUNCTION",
            ObjectType::BuiltinObj => "BUILTIN",
            ObjectType::HashObj => "HASH",
            ObjectType::ErrorObj => "ERROR",
        };
        write!(f, "{}", type_str)
    }
}

#[derive(PartialEq, Clone)]
pub enum ObjectTypes {
    Integer(Integer),
    StringLiteral(StringLiteral),
    Boolean(Boolean),
    Array(Array),
    Null(Null),
    ReturnValue(ReturnValue),
    Error(Error),
    Function(Function),
    Builtin(Builtin),
    Hash(Hash),
}

impl ObjectTypes {
    pub fn type_(&self) -> ObjectType {
        match self {
            ObjectTypes::Integer(integer) => integer.type_(),
            ObjectTypes::StringLiteral(string) => string.type_(),
            ObjectTypes::Boolean(boolean) => boolean.type_(),
            ObjectTypes::Array(array) => array.type_(),
            ObjectTypes::Null(null) => null.type_(),
            ObjectTypes::ReturnValue(return_value) => return_value.type_(),
            ObjectTypes::Error(error) => error.type_(),
            ObjectTypes::Function(function) => function.type_(),
            ObjectTypes::Builtin(builtin) => builtin.type_(),
            ObjectTypes::Hash(hash) => hash.type_(),
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            ObjectTypes::Integer(integer) => integer.inspect(),
            ObjectTypes::StringLiteral(string) => string.inspect(),
            ObjectTypes::Boolean(boolean) => boolean.inspect(),
            ObjectTypes::Array(array) => array.inspect(),
            ObjectTypes::Null(null) => null.inspect(),
            ObjectTypes::ReturnValue(return_value) => return_value.inspect(),
            ObjectTypes::Error(error) => error.inspect(),
            ObjectTypes::Function(function) => function.inspect(),
            ObjectTypes::Builtin(builtin) => builtin.inspect(),
            ObjectTypes::Hash(hash) => hash.inspect(),
        }
    }
}

trait Object {
    fn type_(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct HashKey {
    type_: ObjectType,
    value: u64,
}

#[derive(PartialEq, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn type_(&self) -> ObjectType {
        ObjectType::IntegerObj
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

impl Integer {
    pub fn hash_key(&self) -> HashKey {
        HashKey {
            type_: self.type_(),
            value: self.value as u64,
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct StringLiteral {
    pub value: String,
}

impl Object for StringLiteral {
    fn type_(&self) -> ObjectType {
        ObjectType::StringObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

// FNV-1a 64-bit hash function
// ref: https://ssojet.com/hashing/fnv-1a-in-python/
fn fnv1a_64(data: &str) -> u64 {
    let fnv_prime: u64 = 0x100000001b3;
    let mut hash_val: u64 = 0xcbf29ce484222325;
    for byte in data.as_bytes() {
        hash_val ^= *byte as u64;
        hash_val = hash_val.wrapping_mul(fnv_prime);
    }
    hash_val
}

impl StringLiteral {
    pub fn hash_key(&self) -> HashKey {
        let hash = fnv1a_64(&self.value);
        HashKey {
            type_: self.type_(),
            value: hash,
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn type_(&self) -> ObjectType {
        ObjectType::BooleanObj
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

impl Boolean {
    pub fn hash_key(&self) -> HashKey {
        let value = if self.value { 1 } else { 0 };
        HashKey {
            type_: self.type_(),
            value,
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Array {
    pub elements: Vec<ObjectTypes>,
}

impl Object for Array {
    fn type_(&self) -> ObjectType {
        ObjectType::ArrayObj
    }

    fn inspect(&self) -> String {
        let elements: Vec<String> = self.elements.iter().map(|e| e.inspect()).collect();
        format!("[{}]", elements.join(", "))
    }
}

#[derive(PartialEq, Clone)]
pub struct Null;

impl Object for Null {
    fn type_(&self) -> ObjectType {
        ObjectType::NullObj
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(PartialEq, Clone)]
pub struct ReturnValue {
    pub value: Box<ObjectTypes>,
}

impl Object for ReturnValue {
    fn type_(&self) -> ObjectType {
        ObjectType::ReturnValueObj
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

#[derive(PartialEq, Clone)]
pub struct Error {
    pub message: String,
}

impl Object for Error {
    fn type_(&self) -> ObjectType {
        ObjectType::ErrorObj
    }

    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }
}

#[derive(PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl Object for Function {
    fn type_(&self) -> ObjectType {
        ObjectType::FunctionObj
    }

    fn inspect(&self) -> String {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        format!("fn({}) {{\n{}\n}}", params.join(", "), self.body)
    }
}

pub type BuiltinFunction = fn(&[ObjectTypes]) -> ObjectTypes;

#[derive(PartialEq, Clone)]
pub struct Builtin {
    pub fn_: BuiltinFunction,
}

impl Object for Builtin {
    fn type_(&self) -> ObjectType {
        ObjectType::BuiltinObj
    }

    fn inspect(&self) -> String {
        "builtin function".to_string()
    }
}

#[derive(PartialEq, Clone)]
pub struct HashPair {
    pub key: ObjectTypes,
    pub value: ObjectTypes,
}

#[derive(PartialEq, Clone)]
pub struct Hash {
    pub pairs: HashMap<HashKey, HashPair>,
}

impl Object for Hash {
    fn type_(&self) -> ObjectType {
        ObjectType::HashObj
    }

    fn inspect(&self) -> String {
        let pairs: Vec<String> = self
            .pairs
            .values()
            .map(|pair| format!("{}: {}", pair.key.inspect(), pair.value.inspect()))
            .collect();
        format!("{{{}}}", pairs.join(", "))
    }
}
