use crate::assembly;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Program(pub FunctionDefiniton);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDefiniton {
    pub identifier: String,
    pub body: Vec<Instruction>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var(pub String);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instruction {
    Return(Value),
    Unary {
        operator: UnaryOperator,
        src: Value,
        dst: Var,
    },
    Binary {
        op: BinaryOperator,
        lhs: Value,
        rhs: Value,
        dst: Var,
    },
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
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
