#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Unary(UnaryOperator, Operand),
    AllocateStack(u32),
    Ret,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operand {
    Register(Register),
    Imm(i32),
    Pseudo(String),
    Stack(u32),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
    AX,
    R10,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Program(pub FunctionDefinition);
