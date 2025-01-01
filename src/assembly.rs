//! amd64 assembly

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instruction {
    Mov {
        src: Operand,
        dst: Operand,
    },
    Unary {
        op: UnaryOperator,
        operand: Operand,
    },
    Binary {
        op: BinaryOperator,
        lhs: Operand,
        rhs: Operand,
    },
    Cmp {
        lhs: Operand,
        rhs: Operand,
    },
    Idiv(Operand),
    Cdq,
    Jmp(String),
    JumpCC(CondCode, String),
    SetCC(CondCode, Operand),
    Label(String),
    AllocateStack(usize),
    DeallocateStack(usize),
    Push(Operand),
    Call(String),
    Ret,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    Sal,
    Sar,
    And,
    Xor,
    Or,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operand {
    Register(Register),
    Imm(i32),
    Pseudo(String),
    Stack(i64),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Register {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RegisterSize {
    One,
    Four,
    Eight,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Program(pub Vec<FunctionDefinition>);
