//! Object of Evaluation for the Monkey interpreter.

type ObjectType = String;

pub const INTEGER_OBJ: &str = "INTEGER";
const BOOLEAN_OBJ: &str = "BOOLEAN";
const NULL_OBJ: &str = "NULL";

#[derive(PartialEq)]
pub enum ObjectTypes {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}

impl ObjectTypes {
    pub fn _type(&self) -> ObjectType {
        match self {
            ObjectTypes::Integer(integer) => integer._type(),
            ObjectTypes::Boolean(boolean) => boolean._type(),
            ObjectTypes::Null(null) => null._type(),
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            ObjectTypes::Integer(integer) => integer.inspect(),
            ObjectTypes::Boolean(boolean) => boolean.inspect(),
            ObjectTypes::Null(null) => null.inspect(),
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
