use crate::tacky;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Expression {
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary {
        op: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
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
    BitwiseAnd,
    Xor,
    BitwiseOr,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
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
            BinaryOperator::BitwiseAnd => tacky::BinaryOperator::BitwiseAnd,
            BinaryOperator::Xor => tacky::BinaryOperator::Xor,
            BinaryOperator::BitwiseOr => tacky::BinaryOperator::BitwiseOr,
            BinaryOperator::And => unreachable!("And && can not be a tacky binary operator"),
            BinaryOperator::Or => unreachable!("Or || can not be a tacky binary operator"),
            BinaryOperator::Equal => tacky::BinaryOperator::Equal,
            BinaryOperator::NotEqual => tacky::BinaryOperator::NotEqual,
            BinaryOperator::LessThan => tacky::BinaryOperator::LessThan,
            BinaryOperator::LessOrEqual => tacky::BinaryOperator::LessOrEqual,
            BinaryOperator::GreaterThan => tacky::BinaryOperator::GreaterThan,
            BinaryOperator::GreaterOrEqual => tacky::BinaryOperator::GreaterOrEqual,
        }
    }
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

impl UnaryOperator {
    pub fn to_tacky(&self) -> tacky::UnaryOperator {
        match self {
            UnaryOperator::Complement => tacky::UnaryOperator::Complement,
            UnaryOperator::Negate => tacky::UnaryOperator::Negate,
            UnaryOperator::Not => tacky::UnaryOperator::Not,
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
