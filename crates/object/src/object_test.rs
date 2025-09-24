use crate::object::StringLiteral;

#[test]
fn test_string_hash_key() {
    let s1 = StringLiteral {
        value: "Hello World".to_string(),
    };
    let s2 = StringLiteral {
        value: "Hello World".to_string(),
    };
    let s3 = StringLiteral {
        value: "My name is John".to_string(),
    };

    assert_eq!(s1.hash_key(), s2.hash_key());
    assert_ne!(s1.hash_key(), s3.hash_key());
}
