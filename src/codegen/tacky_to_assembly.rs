use std::ffi::VaList;

use crate::{
    assembly::{self, CondCode, Instruction, Operand, Register},
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
            assembly::Instruction::Unary {
                op: operator.to_assembly(),
                operand: assembly::Operand::Pseudo(dst.0),
            },
        ],
        tacky::Instruction::Binary {
            op:
                tacky::BinaryOperator::GreaterThan
                | tacky::BinaryOperator::GreaterOrEqual
                | tacky::BinaryOperator::LessThan
                | tacky::BinaryOperator::LessOrEqual,
            lhs,
            rhs,
            dst,
        } => vec![],
        tacky::Instruction::Binary {
            op: tacky::BinaryOperator::Divide,
            lhs,
            rhs,
            dst,
        } => vec![
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
        tacky::Instruction::Binary {
            op: tacky::BinaryOperator::Remainder,
            lhs,
            rhs,
            dst,
        } => vec![
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
        tacky::Instruction::Binary { op, lhs, rhs, dst } => vec![
            assembly::Instruction::Mov {
                src: lhs.to_operand(),
                dst: assembly::Operand::Pseudo(dst.0.clone()),
            },
            assembly::Instruction::Binary {
                op: match op {
                    tacky::BinaryOperator::Add => assembly::BinaryOperator::Add,
                    tacky::BinaryOperator::Subtract => assembly::BinaryOperator::Sub,
                    tacky::BinaryOperator::Multiply => assembly::BinaryOperator::Mult,
                    tacky::BinaryOperator::Sal => assembly::BinaryOperator::Sal,
                    tacky::BinaryOperator::Sar => assembly::BinaryOperator::Sar,
                    tacky::BinaryOperator::BitwiseAnd => assembly::BinaryOperator::And,
                    tacky::BinaryOperator::Xor => assembly::BinaryOperator::Xor,
                    tacky::BinaryOperator::BitwiseOr => assembly::BinaryOperator::Or,
                    tacky::BinaryOperator::Divide => unreachable!(),
                    tacky::BinaryOperator::Remainder => unreachable!(),
                    tacky::BinaryOperator::Equal => unreachable!(),
                    tacky::BinaryOperator::NotEqual => unreachable!(),
                    tacky::BinaryOperator::LessThan => unreachable!(),
                    tacky::BinaryOperator::LessOrEqual => unreachable!(),
                    tacky::BinaryOperator::GreaterThan => unreachable!(),
                    tacky::BinaryOperator::GreaterOrEqual => unreachable!(),
                },
                lhs: rhs.to_operand(),
                rhs: assembly::Operand::Pseudo(dst.0),
            },
        ],
        tacky::Instruction::JumpIfZero(val, target) => vec![
            Instruction::Cmp {
                lhs: Operand::Imm(0),
                rhs: val.to_operand(),
            },
            Instruction::JumpCC(CondCode::E, target),
        ],
    }
}
