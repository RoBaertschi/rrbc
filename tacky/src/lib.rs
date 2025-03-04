#[cfg(feature = "ast")]
pub mod ast;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Program(pub Vec<FunctionDefiniton>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionDefiniton {
    pub identifier: String,
    pub params: Vec<String>,
    pub body: Vec<Instruction>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
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
    /// src, dst
    Copy(Value, Value),
    /// Label to jump to.
    Jump(String),
    /// Expression, Label
    JumpIfZero(Value, String),
    /// Expression, Label
    JumpIfNotZero(Value, String),
    /// Identifier
    Label(String),
    FunCall {
        fun_name: String,
        args: Vec<Value>,
        dst: Var,
    },
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Value {
    Constant(i32),
    Var(Var),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Sal,
    Sar,
    BitwiseAnd,
    Xor,
    BitwiseOr,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}
