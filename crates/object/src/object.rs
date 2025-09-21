//! Object of Evaluation for the Monkey interpreter.

use ast::ast::{BlockStatement, Identifier, TNode};

use crate::environment::Environment;

type ObjectType = String;

pub const INTEGER_OBJ: &str = "INTEGER";
pub const STRING_OBJ: &str = "STRING";
const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const ARRAY_OBJ: &str = "ARRAY";
const NULL_OBJ: &str = "NULL";
pub const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";
const FUNCTION_OBJ: &str = "FUNCTION";
const BUILTIN_OBJ: &str = "BUILTIN";
pub const ERROR_OBJ: &str = "ERROR";

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
}

impl ObjectTypes {
    pub fn _type(&self) -> ObjectType {
        match self {
            ObjectTypes::Integer(integer) => integer._type(),
            ObjectTypes::StringLiteral(string) => string._type(),
            ObjectTypes::Boolean(boolean) => boolean._type(),
            ObjectTypes::Array(array) => array._type(),
            ObjectTypes::Null(null) => null._type(),
            ObjectTypes::ReturnValue(return_value) => return_value._type(),
            ObjectTypes::Error(error) => error._type(),
            ObjectTypes::Function(function) => function._type(),
            ObjectTypes::Builtin(builtin) => builtin._type(),
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
        }
    }
}

trait Object {
    fn _type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(PartialEq, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn _type(&self) -> ObjectType {
        INTEGER_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(PartialEq, Clone)]
pub struct StringLiteral {
    pub value: String,
}

impl Object for StringLiteral {
    fn _type(&self) -> ObjectType {
        STRING_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(PartialEq, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn _type(&self) -> ObjectType {
        BOOLEAN_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(PartialEq, Clone)]
pub struct Array {
    pub elements: Vec<ObjectTypes>,
}

impl Object for Array {
    fn _type(&self) -> ObjectType {
        ARRAY_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        let elements: Vec<String> = self.elements.iter().map(|e| e.inspect()).collect();
        format!("[{}]", elements.join(", "))
    }
}

#[derive(PartialEq, Clone)]
pub struct Null;

impl Object for Null {
    fn _type(&self) -> ObjectType {
        NULL_OBJ.to_string()
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
    fn _type(&self) -> ObjectType {
        RETURN_VALUE_OBJ.to_string()
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
    fn _type(&self) -> ObjectType {
        ERROR_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }
}

#[derive(PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Object for Function {
    fn _type(&self) -> ObjectType {
        FUNCTION_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        format!(
            "fn({}) {{\n{}\n}}",
            params.join(", "),
            self.body.to_string()
        )
    }
}

pub type BuiltinFunction = fn(Vec<ObjectTypes>) -> ObjectTypes;

#[derive(PartialEq, Clone)]
pub struct Builtin {
    pub _fn: BuiltinFunction,
}

impl Object for Builtin {
    fn _type(&self) -> ObjectType {
        BUILTIN_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        "builtin function".to_string()
    }
}
