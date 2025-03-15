use rrbc_asm::{self, CondCode, Instruction, Operand, Register};
use rrbc_tacky::{self, UnaryOperator, Value};

pub fn code_generation(program: rrbc_tacky::Program) -> rrbc_asm::Program {
    rrbc_asm::Program(program.0.into_iter().map(cg_function).collect())
}

const ARG_REGISTERS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::DX,
    Register::CX,
    Register::R8,
    Register::R9,
];

fn cg_function(func: rrbc_tacky::FunctionDefiniton) -> rrbc_asm::FunctionDefinition {
    let mut asm_insts: Vec<rrbc_asm::Instruction> = vec![];

    for (i, param) in func.params.into_iter().enumerate() {
        if i < ARG_REGISTERS.len() {
            asm_insts.push(rrbc_asm::Instruction::Mov {
                src: Operand::Register(ARG_REGISTERS[i]),
                dst: Operand::Pseudo(param),
            });
        } else {
            asm_insts.push(rrbc_asm::Instruction::Mov {
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

    rrbc_asm::FunctionDefinition {
        name: func.identifier,
        instructions: asm_insts,
    }
}

fn cg_instruction(instruction: rrbc_tacky::Instruction) -> Vec<rrbc_asm::Instruction> {
    match instruction {
        rrbc_tacky::Instruction::Return(val) => vec![
            rrbc_asm::Instruction::Mov {
                src: val.into(),
                dst: rrbc_asm::Operand::Register(Register::AX),
            },
            rrbc_asm::Instruction::Ret,
        ],
        rrbc_tacky::Instruction::Unary {
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
        rrbc_tacky::Instruction::Unary { operator, src, dst } => vec![
            rrbc_asm::Instruction::Mov {
                src: src.into(),
                dst: rrbc_asm::Operand::Pseudo(dst.0.clone()),
            },
            rrbc_asm::Instruction::Unary {
                op: operator.try_into().expect("Not already handled above"),
                operand: rrbc_asm::Operand::Pseudo(dst.0),
            },
        ],
        rrbc_tacky::Instruction::Binary {
            op:
                operand @ (rrbc_tacky::BinaryOperator::GreaterThan
                | rrbc_tacky::BinaryOperator::GreaterOrEqual
                | rrbc_tacky::BinaryOperator::LessThan
                | rrbc_tacky::BinaryOperator::LessOrEqual
                | rrbc_tacky::BinaryOperator::Equal
                | rrbc_tacky::BinaryOperator::NotEqual),
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
                dst: rrbc_asm::Operand::Pseudo(dst.0.clone()),
            },
            Instruction::SetCC(
                match operand {
                    rrbc_tacky::BinaryOperator::LessThan => CondCode::L,
                    rrbc_tacky::BinaryOperator::LessOrEqual => CondCode::LE,
                    rrbc_tacky::BinaryOperator::GreaterThan => CondCode::G,
                    rrbc_tacky::BinaryOperator::GreaterOrEqual => CondCode::GE,
                    rrbc_tacky::BinaryOperator::Equal => CondCode::E,
                    rrbc_tacky::BinaryOperator::NotEqual => CondCode::NE,
                    op => unreachable!(
                        "unreachable in rrbc_asm generation for relational operators: {:?}",
                        op
                    ),
                },
                rrbc_asm::Operand::Pseudo(dst.0),
            ),
        ],
        rrbc_tacky::Instruction::Binary {
            op: rrbc_tacky::BinaryOperator::Divide,
            lhs,
            rhs,
            dst,
        } => vec![
            rrbc_asm::Instruction::Mov {
                src: lhs.into(),
                dst: rrbc_asm::Operand::Register(Register::AX),
            },
            rrbc_asm::Instruction::Cdq,
            rrbc_asm::Instruction::Idiv(rhs.into()),
            rrbc_asm::Instruction::Mov {
                src: rrbc_asm::Operand::Register(Register::AX),
                dst: rrbc_asm::Operand::Pseudo(dst.0),
            },
        ],
        rrbc_tacky::Instruction::Binary {
            op: rrbc_tacky::BinaryOperator::Remainder,
            lhs,
            rhs,
            dst,
        } => vec![
            rrbc_asm::Instruction::Mov {
                src: lhs.into(),
                dst: rrbc_asm::Operand::Register(Register::AX),
            },
            rrbc_asm::Instruction::Cdq,
            rrbc_asm::Instruction::Idiv(rhs.into()),
            rrbc_asm::Instruction::Mov {
                src: rrbc_asm::Operand::Register(Register::DX),
                dst: rrbc_asm::Operand::Pseudo(dst.0),
            },
        ],
        rrbc_tacky::Instruction::Binary { op, lhs, rhs, dst } => vec![
            rrbc_asm::Instruction::Mov {
                src: lhs.into(),
                dst: rrbc_asm::Operand::Pseudo(dst.0.clone()),
            },
            rrbc_asm::Instruction::Binary {
                op: match op {
                    rrbc_tacky::BinaryOperator::Add => rrbc_asm::BinaryOperator::Add,
                    rrbc_tacky::BinaryOperator::Subtract => rrbc_asm::BinaryOperator::Sub,
                    rrbc_tacky::BinaryOperator::Multiply => rrbc_asm::BinaryOperator::Mult,
                    rrbc_tacky::BinaryOperator::Sal => rrbc_asm::BinaryOperator::Sal,
                    rrbc_tacky::BinaryOperator::Sar => rrbc_asm::BinaryOperator::Sar,
                    rrbc_tacky::BinaryOperator::BitwiseAnd => rrbc_asm::BinaryOperator::And,
                    rrbc_tacky::BinaryOperator::Xor => rrbc_asm::BinaryOperator::Xor,
                    rrbc_tacky::BinaryOperator::BitwiseOr => rrbc_asm::BinaryOperator::Or,
                    rrbc_tacky::BinaryOperator::Divide => unreachable!(),
                    rrbc_tacky::BinaryOperator::Remainder => unreachable!(),
                    rrbc_tacky::BinaryOperator::Equal => unreachable!(),
                    rrbc_tacky::BinaryOperator::NotEqual => unreachable!(),
                    rrbc_tacky::BinaryOperator::LessThan => unreachable!(),
                    rrbc_tacky::BinaryOperator::LessOrEqual => unreachable!(),
                    rrbc_tacky::BinaryOperator::GreaterThan => unreachable!(),
                    rrbc_tacky::BinaryOperator::GreaterOrEqual => unreachable!(),
                },
                lhs: rhs.into(),
                rhs: rrbc_asm::Operand::Pseudo(dst.0),
            },
        ],
        rrbc_tacky::Instruction::JumpIfZero(val, target) => vec![
            Instruction::Cmp {
                lhs: Operand::Imm(0),
                rhs: val.into(),
            },
            Instruction::JumpCC(CondCode::E, target),
        ],
        rrbc_tacky::Instruction::JumpIfNotZero(val, target) => vec![
            Instruction::Cmp {
                lhs: Operand::Imm(0),
                rhs: val.into(),
            },
            Instruction::JumpCC(CondCode::NE, target),
        ],
        rrbc_tacky::Instruction::Copy(src, dst) => vec![Instruction::Mov {
            src: src.into(),
            dst: dst.into(),
        }],
        rrbc_tacky::Instruction::Jump(label) => vec![Instruction::Jmp(label)],
        rrbc_tacky::Instruction::Label(label) => vec![Instruction::Label(label)],
        rrbc_tacky::Instruction::FunCall {
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
                let rrbc_asm_arg = arg.into();
                match rrbc_asm_arg {
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

            let rrbc_asm_dst = Value::Var(dst).into();
            instructions.push(Instruction::Mov {
                src: Operand::Register(Register::AX),
                dst: rrbc_asm_dst,
            });

            instructions
        }
    }
}
