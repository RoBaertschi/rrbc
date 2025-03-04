use std::collections::HashMap;

pub type Identifier = String;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Expression {
    Var(Identifier),
    Constant(i32),
    Unary {
        op: UnaryOperator,
        expression: Box<Expression>,
    },
    Binary {
        op: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Assignment {
        op: AssignmentOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Conditional {
        condition: Box<Expression>,
        then: Box<Expression>,
        r#else: Box<Expression>,
    },
    Postfix(PostfixOperator, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
}

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum PostfixOperator {
    Increment,
    Decrement,
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
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

impl AssignmentOperator {
    #[cfg(feature = "tacky")]
    pub fn to_tacky(&self) -> tacky::BinaryOperator {
        match self {
            AssignmentOperator::None => unreachable!(
                "It is not possible to create a binary operator from a None AssignmentOperator"
            ),
            AssignmentOperator::Add => tacky::BinaryOperator::Add,
            AssignmentOperator::Subtract => tacky::BinaryOperator::Subtract,
            AssignmentOperator::Multiply => tacky::BinaryOperator::Multiply,
            AssignmentOperator::Divide => tacky::BinaryOperator::Divide,
            AssignmentOperator::Reminder => tacky::BinaryOperator::Remainder,
            AssignmentOperator::BitwiseOr => tacky::BinaryOperator::BitwiseOr,
            AssignmentOperator::BitwiseAnd => tacky::BinaryOperator::BitwiseAnd,
            AssignmentOperator::BitwiseXor => tacky::BinaryOperator::Xor,
            AssignmentOperator::ShiftLeft => tacky::BinaryOperator::Sal,
            AssignmentOperator::ShiftRight => tacky::BinaryOperator::Sar,
        }
    }
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

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
    Increment,
    Decrement,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If {
        condition: Expression,
        then: Box<Statement>,
        r#else: Option<Box<Statement>>,
    },
    Goto(Identifier),
    Label(Identifier, Box<Statement>),
    Default(Box<Statement>, Identifier),
    // You should check if Expression is constant time. Which should be pretty easy.
    Case(Expression, Box<Statement>, Identifier),
    Compound(Block),
    Break(Identifier),
    Continue(Identifier),
    While {
        condition: Expression,
        body: Box<Statement>,
        label: Option<Identifier>,
    },
    DoWhile {
        body: Box<Statement>,
        condition: Expression,
        label: Option<Identifier>,
    },
    For {
        init: ForInit,
        condition: Option<Expression>,
        post: Option<Expression>,
        body: Box<Statement>,
        label: Option<Identifier>,
    },
    Switch {
        expression: Expression,
        body: Box<Statement>,
        label: Option<Identifier>,
        /// This data will only be populated after the coresspondig pass is done. So only use this
        /// in the that specific pass or in tacky generation.
        cases: Option<HashMap<Option<i32>, String>>,
    },
    Null,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ForInit {
    InitDecl(VariableDeclaration),
    InitExp(Expression),
    None,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Declaration {
    FunDecl(FunctionDeclaration),
    VarDecl(VariableDeclaration),
}

#[derive(Debug, Eq, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub exp: Option<Expression>,
}

#[derive(Debug, Eq, PartialEq)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block(pub Vec<BlockItem>);

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: Option<Block>,
}

#[derive(Debug)]
pub struct Program {
    pub function_declarations: Vec<FunctionDeclaration>,
}
