//! Object of Evaluation for the Monkey interpreter.

type ObjectType = String;

pub const INTEGER_OBJ: &str = "INTEGER";
const BOOLEAN_OBJ: &str = "BOOLEAN";
const NULL_OBJ: &str = "NULL";
pub const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";

#[derive(PartialEq)]
pub enum ObjectTypes {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    ReturnValue(ReturnValue),
}

impl ObjectTypes {
    pub fn _type(&self) -> ObjectType {
        match self {
            ObjectTypes::Integer(integer) => integer._type(),
            ObjectTypes::Boolean(boolean) => boolean._type(),
            ObjectTypes::Null(null) => null._type(),
            ObjectTypes::ReturnValue(return_value) => return_value._type(),
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            ObjectTypes::Integer(integer) => integer.inspect(),
            ObjectTypes::Boolean(boolean) => boolean.inspect(),
            ObjectTypes::Null(null) => null.inspect(),
            ObjectTypes::ReturnValue(return_value) => return_value.inspect(),
        }
    }
}

trait Object {
    fn _type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(PartialEq)]
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

#[derive(PartialEq)]
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

#[derive(PartialEq)]
pub struct Null;

impl Object for Null {
    fn _type(&self) -> ObjectType {
        NULL_OBJ.to_string()
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(PartialEq)]
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
