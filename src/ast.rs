#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Expression {
    Constant(i32),
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub body: Statement,
}

#[derive(Debug)]
pub struct Program {
    pub function_definition: FunctionDefinition,
}
