use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::object::ObjectTypes;

pub fn new_enclosed_environment(outer: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
    let env = new_environment();
    env.borrow_mut().outer = Some(outer);
    env
}

pub fn new_environment() -> Rc<RefCell<Environment>> {
    Rc::new(RefCell::new(Environment {
        store: HashMap::new(),
        outer: None,
    }))
}

#[derive(PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, ObjectTypes>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn get(&self, name: &str) -> Option<ObjectTypes> {
        let obj = self.store.get(name).cloned();
        if obj.is_none() && self.outer.is_some() {
            return self.outer.as_ref().unwrap().borrow().get(name);
        }
        obj
    }

    pub fn set(&mut self, name: &str, val: ObjectTypes) {
        self.store.insert(name.to_string(), val);
    }
}
