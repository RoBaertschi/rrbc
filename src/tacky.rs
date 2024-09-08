use crate::assembly;

#[derive(Debug)]
pub struct Program(pub FunctionDefiniton);

#[derive(Debug)]
pub struct FunctionDefiniton {
    pub identifier: String,
    pub body: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Var(pub String);

#[derive(Debug)]
pub enum Instruction {
    Return(Value),
    Unary {
        operator: UnaryOperator,
        src: Value,
        dst: Var,
    },
}

#[derive(Debug)]
pub enum Value {
    Constant(i32),
    Var(Var),
}

impl Value {
    pub fn to_operand(&self) -> assembly::Operand {
        match self {
            Value::Constant(val) => assembly::Operand::Imm(*val),
            Value::Var(ident) => assembly::Operand::Pseudo(ident.0.clone()),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

impl UnaryOperator {
    pub fn to_assembly(&self) -> assembly::UnaryOperator {
        match self {
            UnaryOperator::Complement => assembly::UnaryOperator::Not,
            UnaryOperator::Negate => assembly::UnaryOperator::Neg,
        }
    }
}
