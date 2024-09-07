#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret(),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operand {
    Register(Register),
    Imm(i32),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Register {
    EAX,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Program(pub FunctionDefinition);
