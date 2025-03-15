use crate::{
    assembly::{self, CondCode, Instruction, Operand, Register},
    tacky::{self, UnaryOperator, Value},
};

pub(super) fn code_generation(program: tacky::Program) -> assembly::Program {
    assembly::Program(program.0.into_iter().map(cg_function).collect())
}

const ARG_REGISTERS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::DX,
    Register::CX,
    Register::R8,
    Register::R9,
];

pub(super) fn cg_function(func: tacky::FunctionDefiniton) -> assembly::FunctionDefinition {
    let mut asm_insts: Vec<assembly::Instruction> = vec![];

    for (i, param) in func.params.into_iter().enumerate() {
        if i < ARG_REGISTERS.len() {
            asm_insts.push(assembly::Instruction::Mov {
                src: Operand::Register(ARG_REGISTERS[i]),
                dst: Operand::Pseudo(param),
            });
        } else {
            asm_insts.push(assembly::Instruction::Mov {
                src: Operand::Stack(16_i64 + (8_i64 * <usize as TryInto<i64>>
                    ::try_into(i - ARG_REGISTERS.len())
                    .expect("Stack has grown to big to fit into a i64, this is highly unlikely, if this happens to you please open an issue"))),
                dst: Operand::Pseudo(param),
            });
        }
    }

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
                src: val.into(),
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
                rhs: src.into(),
            },
            Instruction::Mov {
                src: Operand::Imm(0),
                dst: Operand::Pseudo(dst.0.clone()),
            },
            Instruction::SetCC(CondCode::E, Operand::Pseudo(dst.0)),
        ],
        tacky::Instruction::Unary { operator, src, dst } => vec![
            assembly::Instruction::Mov {
                src: src.into(),
                dst: assembly::Operand::Pseudo(dst.0.clone()),
            },
            assembly::Instruction::Unary {
                op: operator.try_into().expect("Not already handled above"),
                operand: assembly::Operand::Pseudo(dst.0),
            },
        ],
        tacky::Instruction::Binary {
            op:
                operand @ (tacky::BinaryOperator::GreaterThan
                | tacky::BinaryOperator::GreaterOrEqual
                | tacky::BinaryOperator::LessThan
                | tacky::BinaryOperator::LessOrEqual
                | tacky::BinaryOperator::Equal
                | tacky::BinaryOperator::NotEqual),
            lhs,
            rhs,
            dst,
        } => vec![
            Instruction::Cmp {
                lhs: rhs.into(),
                rhs: lhs.into(),
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
                src: lhs.into(),
                dst: assembly::Operand::Register(Register::AX),
            },
            assembly::Instruction::Cdq,
            assembly::Instruction::Idiv(rhs.into()),
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
                src: lhs.into(),
                dst: assembly::Operand::Register(Register::AX),
            },
            assembly::Instruction::Cdq,
            assembly::Instruction::Idiv(rhs.into()),
            assembly::Instruction::Mov {
                src: assembly::Operand::Register(Register::DX),
                dst: assembly::Operand::Pseudo(dst.0),
            },
        ],
        tacky::Instruction::Binary { op, lhs, rhs, dst } => vec![
            assembly::Instruction::Mov {
                src: lhs.into(),
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
                lhs: rhs.into(),
                rhs: assembly::Operand::Pseudo(dst.0),
            },
        ],
        tacky::Instruction::JumpIfZero(val, target) => vec![
            Instruction::Cmp {
                lhs: Operand::Imm(0),
                rhs: val.into(),
            },
            Instruction::JumpCC(CondCode::E, target),
        ],
        tacky::Instruction::JumpIfNotZero(val, target) => vec![
            Instruction::Cmp {
                lhs: Operand::Imm(0),
                rhs: val.into(),
            },
            Instruction::JumpCC(CondCode::NE, target),
        ],
        tacky::Instruction::Copy(src, dst) => vec![Instruction::Mov {
            src: src.into(),
            dst: dst.into(),
        }],
        tacky::Instruction::Jump(label) => vec![Instruction::Jmp(label)],
        tacky::Instruction::Label(label) => vec![Instruction::Label(label)],
        tacky::Instruction::FunCall {
            fun_name,
            args,
            dst,
        } => {
            let (register_args, stack_args) = args
                .split_at_checked(ARG_REGISTERS.len())
                .unwrap_or((&args, &[]));

            let stack_padding: usize = if stack_args.len() % 2 == 0 { 0 } else { 8 };
            let mut instructions = vec![];

            if stack_padding != 0 {
                instructions.push(Instruction::AllocateStack(stack_padding));
            }

            for (i, arg) in register_args.iter().enumerate() {
                instructions.push(Instruction::Mov {
                    src: arg.into(),
                    dst: Operand::Register(ARG_REGISTERS[i]),
                });
            }

            for arg in stack_args.iter().rev() {
                let assembly_arg = arg.into();
                match assembly_arg {
                    arg @ (Operand::Register(_) | Operand::Imm(_)) => {
                        instructions.push(Instruction::Push(arg))
                    }
                    arg @ (Operand::Pseudo(_) | Operand::Stack(_)) => {
                        instructions.push(Instruction::Mov {
                            src: arg,
                            dst: Operand::Register(Register::AX),
                        });
                        instructions.push(Instruction::Push(Operand::Register(Register::AX)));
                    }
                }
            }

            instructions.push(Instruction::Call(fun_name));
            let bytes_to_remove = 8 * stack_args.len() + stack_padding;
            if bytes_to_remove != 0 {
                instructions.push(Instruction::DeallocateStack(bytes_to_remove));
            }

            let assembly_dst = Value::Var(dst).into();
            instructions.push(Instruction::Mov {
                src: Operand::Register(Register::AX),
                dst: assembly_dst,
            });

            instructions
        }
    }
}
