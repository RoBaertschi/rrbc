use crate::{
    assembly::{self, Register},
    tacky,
};

pub(super) fn code_generation(program: tacky::Program) -> assembly::Program {
    assembly::Program(cg_function(program.0))
}

pub(super) fn cg_function(func: tacky::FunctionDefiniton) -> assembly::FunctionDefinition {
    let mut asm_insts: Vec<assembly::Instruction> = vec![];

    for inst in func.body {
        asm_insts.append(&mut cg_instruction(inst));
    }

    assembly::FunctionDefinition {
        name: func.identifier,
        instructions: asm_insts,
    }
}

pub(super) fn cg_instruction(instruction: tacky::Instruction) -> Vec<assembly::Instruction> {
    match instruction {
        tacky::Instruction::Return(val) => vec![
            assembly::Instruction::Mov {
                src: val.to_operand(),
                dst: assembly::Operand::Register(Register::AX),
            },
            assembly::Instruction::Ret,
        ],
        tacky::Instruction::Unary { operator, src, dst } => vec![
            assembly::Instruction::Mov {
                src: src.to_operand(),
                dst: assembly::Operand::Pseudo(dst.0.clone()),
            },
            assembly::Instruction::Unary(operator.to_assembly(), assembly::Operand::Pseudo(dst.0)),
        ],
        tacky::Instruction::Binary { op, lhs, rhs, dst } => match op {
            tacky::BinaryOperator::Divide => vec![
                assembly::Instruction::Mov {
                    src: lhs.to_operand(),
                    dst: assembly::Operand::Register(Register::AX),
                },
                assembly::Instruction::Cdq,
                assembly::Instruction::Idiv(rhs.to_operand()),
                assembly::Instruction::Mov {
                    src: assembly::Operand::Register(Register::AX),
                    dst: assembly::Operand::Pseudo(dst.0),
                },
            ],
            tacky::BinaryOperator::Remainder => vec![
                assembly::Instruction::Mov {
                    src: lhs.to_operand(),
                    dst: assembly::Operand::Register(Register::AX),
                },
                assembly::Instruction::Cdq,
                assembly::Instruction::Idiv(rhs.to_operand()),
                assembly::Instruction::Mov {
                    src: assembly::Operand::Register(Register::DX),
                    dst: assembly::Operand::Pseudo(dst.0),
                },
            ],
            binary_opt => vec![
                assembly::Instruction::Mov {
                    src: lhs.to_operand(),
                    dst: assembly::Operand::Pseudo(dst.0.clone()),
                },
                assembly::Instruction::Binary(
                    match binary_opt {
                        tacky::BinaryOperator::Add => assembly::BinaryOperator::Add,
                        tacky::BinaryOperator::Subtract => assembly::BinaryOperator::Sub,
                        tacky::BinaryOperator::Multiply => assembly::BinaryOperator::Mult,
                        tacky::BinaryOperator::Divide => unreachable!(),
                        tacky::BinaryOperator::Remainder => unreachable!(),
                    },
                    rhs.to_operand(),
                    assembly::Operand::Pseudo(dst.0),
                ),
            ],
        },
    }
}
