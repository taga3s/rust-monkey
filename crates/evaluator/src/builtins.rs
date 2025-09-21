use object::object::{Builtin, Null, ObjectTypes, STRING_OBJ};

fn len_builtin(args: Vec<ObjectTypes>) -> ObjectTypes {
    if args.len() != 1 {
        return ObjectTypes::Error(object::object::Error {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match args[0]._type().as_str() {
        STRING_OBJ => {
            if let ObjectTypes::StringLiteral(string) = &args[0] {
                ObjectTypes::Integer(object::object::Integer {
                    value: string.value.len() as i64,
                })
            } else {
                ObjectTypes::Null(Null)
            }
        }
        _ => ObjectTypes::Error(object::object::Error {
            message: format!("argument to `len` not supported, got {}", args[0]._type()),
        }),
    }
}

pub const BUILTINS: [(&str, ObjectTypes); 1] =
    [("len", ObjectTypes::Builtin(Builtin { _fn: len_builtin }))];
