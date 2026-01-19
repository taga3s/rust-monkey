use object::object::{Builtin, ObjectTypes};

pub const BUILTINS: [(&str, ObjectTypes); 6] = [
    ("len", ObjectTypes::Builtin(Builtin { fn_: len_builtin })),
    (
        "first",
        ObjectTypes::Builtin(Builtin { fn_: first_builtin }),
    ),
    ("last", ObjectTypes::Builtin(Builtin { fn_: last_builtin })),
    ("rest", ObjectTypes::Builtin(Builtin { fn_: rest_builtin })),
    ("push", ObjectTypes::Builtin(Builtin { fn_: push_builtin })),
    ("log", ObjectTypes::Builtin(Builtin { fn_: puts_log })),
];

fn len_builtin(args: &[ObjectTypes]) -> ObjectTypes {
    if args.len() != 1 {
        return ObjectTypes::Error(object::object::Error {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match &args[0] {
        ObjectTypes::StringLiteral(string) => ObjectTypes::Integer(object::object::Integer {
            value: string.value.len() as i64,
        }),
        ObjectTypes::Array(array) => ObjectTypes::Integer(object::object::Integer {
            value: array.elements.len() as i64,
        }),
        _ => ObjectTypes::Error(object::object::Error {
            message: format!("argument to `len` not supported, got {}", args[0].ty()),
        }),
    }
}

fn first_builtin(args: &[ObjectTypes]) -> ObjectTypes {
    if args.len() != 1 {
        return ObjectTypes::Error(object::object::Error {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match &args[0] {
        ObjectTypes::Array(array) => {
            if !array.elements.is_empty() {
                array.elements[0].clone()
            } else {
                ObjectTypes::Null(object::object::Null {})
            }
        }
        _ => ObjectTypes::Error(object::object::Error {
            message: format!("argument to `first` must be ARRAY, got {}", args[0].ty()),
        }),
    }
}

fn last_builtin(args: &[ObjectTypes]) -> ObjectTypes {
    if args.len() != 1 {
        return ObjectTypes::Error(object::object::Error {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match &args[0] {
        ObjectTypes::Array(array) => {
            if !array.elements.is_empty() {
                array.elements[array.elements.len() - 1].clone()
            } else {
                ObjectTypes::Null(object::object::Null {})
            }
        }
        _ => ObjectTypes::Error(object::object::Error {
            message: format!("argument to `last` must be ARRAY, got {}", args[0].ty()),
        }),
    }
}

fn rest_builtin(args: &[ObjectTypes]) -> ObjectTypes {
    if args.len() != 1 {
        return ObjectTypes::Error(object::object::Error {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match &args[0] {
        ObjectTypes::Array(array) => {
            if !array.elements.is_empty() {
                let new_elements = array.elements[1..].to_vec();
                ObjectTypes::Array(object::object::Array {
                    elements: new_elements,
                })
            } else {
                ObjectTypes::Null(object::object::Null {})
            }
        }
        _ => ObjectTypes::Error(object::object::Error {
            message: format!("argument to `rest` must be ARRAY, got {}", args[0].ty()),
        }),
    }
}

fn push_builtin(args: &[ObjectTypes]) -> ObjectTypes {
    if args.len() != 2 {
        return ObjectTypes::Error(object::object::Error {
            message: format!("wrong number of arguments. got={}, want=2", args.len()),
        });
    }

    match &args[0] {
        ObjectTypes::Array(array) => {
            let mut new_elements = array.elements.clone();
            new_elements.push(args[1].clone());
            ObjectTypes::Array(object::object::Array {
                elements: new_elements,
            })
        }
        _ => ObjectTypes::Error(object::object::Error {
            message: format!("argument to `push` must be ARRAY, got {}", args[0].ty()),
        }),
    }
}

fn puts_log(args: &[ObjectTypes]) -> ObjectTypes {
    for arg in args {
        println!("{}", arg.inspect());
    }
    ObjectTypes::Null(object::object::Null {})
}
