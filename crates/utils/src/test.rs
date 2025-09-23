#[derive(Clone)]
pub enum TestLiteral {
    Int(i64),
    Str(&'static str),
    Bool(bool),
    Array(Vec<TestLiteral>),
}
