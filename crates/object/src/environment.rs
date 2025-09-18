use std::collections::HashMap;

use crate::object::ObjectTypes;

pub fn new_environment() -> Environment {
    Environment {
        store: HashMap::new(),
    }
}

pub struct Environment {
    store: HashMap<String, ObjectTypes>,
}

impl Environment {
    pub fn get(&self, name: &str) -> Option<&ObjectTypes> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: String, val: ObjectTypes) -> ObjectTypes {
        self.store.insert(name, val.clone());
        val
    }
}
