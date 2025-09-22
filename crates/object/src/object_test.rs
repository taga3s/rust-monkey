use crate::object::{ObjectTypes, StringLiteral};

#[test]
fn test_string_hash_key() {
    let s1 = ObjectTypes::StringLiteral(StringLiteral {
        value: "Hello World".to_string(),
    });
    let s2 = ObjectTypes::StringLiteral(StringLiteral {
        value: "Hello World".to_string(),
    });
    let s3 = ObjectTypes::StringLiteral(StringLiteral {
        value: "My name is John".to_string(),
    });

    let s1_string = if let ObjectTypes::StringLiteral(s) = s1 {
        s
    } else {
        panic!("Expected StringLiteral")
    };
    let s2_string = if let ObjectTypes::StringLiteral(s) = s2 {
        s
    } else {
        panic!("Expected StringLiteral")
    };
    let s3_string = if let ObjectTypes::StringLiteral(s) = s3 {
        s
    } else {
        panic!("Expected StringLiteral")
    };

    assert_eq!(s1_string.hash_key(), s2_string.hash_key());
    assert_ne!(s1_string.hash_key(), s3_string.hash_key());
}
