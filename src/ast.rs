use crate::tacky;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Reminder,
    ShiftLeft,
    ShiftRight,
    And,
    Xor,
    Or,
}

impl BinaryOperator {
    pub fn to_tacky(&self) -> tacky::BinaryOperator {
        match self {
            BinaryOperator::Add => tacky::BinaryOperator::Add,
            BinaryOperator::Subtract => tacky::BinaryOperator::Subtract,
            BinaryOperator::Multiply => tacky::BinaryOperator::Multiply,
            BinaryOperator::Divide => tacky::BinaryOperator::Divide,
            BinaryOperator::Reminder => tacky::BinaryOperator::Remainder,
            BinaryOperator::ShiftLeft => tacky::BinaryOperator::Sal,
            BinaryOperator::ShiftRight => tacky::BinaryOperator::Sar,
            BinaryOperator::And => tacky::BinaryOperator::And,
            BinaryOperator::Xor => tacky::BinaryOperator::Xor,
            BinaryOperator::Or => tacky::BinaryOperator::Or,
        }
    }
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

impl UnaryOperator {
    pub fn to_tacky(&self) -> tacky::UnaryOperator {
        match self {
            UnaryOperator::Complement => tacky::UnaryOperator::Complement,
            UnaryOperator::Negate => tacky::UnaryOperator::Negate,
        }
    }
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
