pub enum Expression {
    Constant(u32),
}

pub enum Statement {
    Return(Expression),
}

pub struct FunctionDefinition {
    pub name: String,
    pub body: Statement,
}

pub struct Program {
    pub function_definition: FunctionDefinition,
}
