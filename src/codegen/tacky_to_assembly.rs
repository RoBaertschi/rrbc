use crate::{
    assembly::{self, CondCode, Instruction, Operand, Register},
    tacky::{self, UnaryOperator},
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
        tacky::Instruction::Unary {
            operator: UnaryOperator::Not,
            src,
            dst,
        } => vec![
            Instruction::Cmp {
                lhs: Operand::Imm(0),
                rhs: src.to_operand(),
            },
            Instruction::Mov {
                src: Operand::Imm(0),
                dst: Operand::Pseudo(dst.0.clone()),
            },
            Instruction::SetCC(CondCode::E, Operand::Pseudo(dst.0)),
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
                operand @ tacky::BinaryOperator::GreaterThan
                | operand @ tacky::BinaryOperator::GreaterOrEqual
                | operand @ tacky::BinaryOperator::LessThan
                | operand @ tacky::BinaryOperator::LessOrEqual
                | operand @ tacky::BinaryOperator::Equal
                | operand @ tacky::BinaryOperator::NotEqual,
            lhs,
            rhs,
            dst,
        } => vec![
            Instruction::Cmp {
                lhs: rhs.to_operand(),
                rhs: lhs.to_operand(),
            },
            Instruction::Mov {
                src: Operand::Imm(0),
                dst: assembly::Operand::Pseudo(dst.0.clone()),
            },
            Instruction::SetCC(
                match operand {
                    tacky::BinaryOperator::LessThan => CondCode::L,
                    tacky::BinaryOperator::LessOrEqual => CondCode::LE,
                    tacky::BinaryOperator::GreaterThan => CondCode::G,
                    tacky::BinaryOperator::GreaterOrEqual => CondCode::GE,
                    tacky::BinaryOperator::Equal => CondCode::E,
                    tacky::BinaryOperator::NotEqual => CondCode::NE,
                    op => unreachable!(
                        "unreachable in assembly generation for relational operators: {:?}",
                        op
                    ),
                },
                assembly::Operand::Pseudo(dst.0),
            ),
        ],
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
        tacky::Instruction::JumpIfNotZero(val, target) => vec![
            Instruction::Cmp {
                lhs: Operand::Imm(0),
                rhs: val.to_operand(),
            },
            Instruction::JumpCC(CondCode::NE, target),
        ],
        tacky::Instruction::Copy(src, dst) => vec![Instruction::Mov {
            src: src.to_operand(),
            dst: dst.to_operand(),
        }],
        tacky::Instruction::Jump(label) => vec![Instruction::Jmp(label)],
        tacky::Instruction::Label(label) => vec![Instruction::Label(label)],
    }
}
