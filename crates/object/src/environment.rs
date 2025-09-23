use std::collections::HashMap;

use crate::object::ObjectTypes;

pub fn new_enclosed_environment(outer: Environment) -> Environment {
    let mut env = new_environment();
    env.outer = Some(Box::new(outer));
    env
}

pub fn new_environment() -> Environment {
    Environment {
        store: HashMap::new(),
        outer: None,
    }
}

#[derive(PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, ObjectTypes>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn get(&self, name: &str) -> Option<&ObjectTypes> {
        let obj = self.store.get(name);
        if obj.is_none() && self.outer.is_some() {
            return self.outer.as_ref().unwrap().get(name);
        }
        obj
    }

    pub fn set(&mut self, name: String, val: ObjectTypes) {
        self.store.insert(name, val);
    }
}
