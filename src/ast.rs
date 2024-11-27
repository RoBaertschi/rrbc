#[cfg(feature = "tacky")]
use crate::tacky;

type Identifier = String;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Expression {
    Var(Identifier),
    Constant(i32),
    Unary(UnaryOperator, Box<Expression>),
    Binary {
        op: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Assignment {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum AssignmentOperator {
    None,
    Add,
    Subtract,
    Multiply,
    Divide,
    Reminder,
    BitwiseAnd,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
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
    #[cfg(feature = "tacky")]
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
    #[cfg(feature = "tacky")]
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
    Expression(Expression),
    Null,
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Declaration {
    pub name: Identifier,
    pub exp: Option<Expression>,
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: Identifier,
    pub body: Vec<BlockItem>,
}

#[derive(Debug)]
pub struct Program {
    pub function_definition: FunctionDefinition,
}
