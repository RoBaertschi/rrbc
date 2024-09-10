//use crate::tacky;

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
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

//impl UnaryOperator {
//    pub fn to_tacky(&self) -> tacky::UnaryOperator {
//        match self {
//            UnaryOperator::Complement => tacky::UnaryOperator::Complement,
//            UnaryOperator::Negate => tacky::UnaryOperator::Negate,
//        }
//    }
//}

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
