use std::collections::HashMap;

use crate::assembly::{
    BinaryOperator, FunctionDefinition, Instruction, Operand, Program, Register,
};

use rrbc_utils;

pub fn fixup_program(program: Program, stack_offset: HashMap<String, i64>) -> Program {
    Program(
        program
            .0
            .into_iter()
            .map(|func| {
                let offset = stack_offset.get(&func.name).copied().unwrap_or(0);
                fixup_function(func, offset)
            })
            .collect(),
    )
}

fn fixup_function(func: FunctionDefinition, stack_offset: i64) -> FunctionDefinition {
    let stack_bytes = -stack_offset;

    let mut new_instructions = if stack_offset != 0 {
        vec![Instruction::AllocateStack(
            // TODO(Robin): Fix this one day correctly
            // NOTE(Robin): 05.03.2025 past robin, wtf did you mean by that
            rrbc_utils::round_away_from_zero(16, stack_bytes)
                .try_into()
                .expect("stack_bytes is invalid usize"), //rrbc_utils::round_away_from_zero(16, stack_bytes)
                                                         //    .try_into()
                                                         //    .unwrap_or_else(|_err| (-stack_bytes).try_into().unwrap_or(0)),
        )]
    } else {
        vec![]
    };

    new_instructions.append(&mut fixup_instructions(func.instructions));

    FunctionDefinition {
        name: func.name,
        instructions: new_instructions,
    }
}

fn fixup_instructions(instructions: Vec<Instruction>) -> Vec<Instruction> {
    let mut new_instructions = vec![];

    for inst in instructions {
        match inst {
            Instruction::Mov { src, dst } => match (src, dst) {
                (Operand::Stack(val1), Operand::Stack(val2)) => {
                    new_instructions.push(Instruction::Mov {
                        src: Operand::Stack(val1),
                        dst: Operand::Register(crate::assembly::Register::R10),
                    });
                    new_instructions.push(Instruction::Mov {
                        src: Operand::Register(crate::assembly::Register::R10),
                        dst: Operand::Stack(val2),
                    });
                }
                (src, dst) => new_instructions.push(Instruction::Mov { src, dst }),
            },
            Instruction::Binary {
                op: BinaryOperator::Mult,
                lhs,
                rhs: Operand::Stack(rhs),
            } => {
                new_instructions.push(Instruction::Mov {
                    src: Operand::Stack(rhs),
                    dst: Operand::Register(Register::R11),
                });
                new_instructions.push(Instruction::Binary {
                    op: BinaryOperator::Mult,
                    lhs,
                    rhs: Operand::Register(Register::R11),
                });
                new_instructions.push(Instruction::Mov {
                    src: Operand::Register(Register::R11),
                    dst: Operand::Stack(rhs),
                });
            }
            Instruction::Binary {
                op: BinaryOperator::Sal,
                lhs: Operand::Stack(lhs),
                rhs: Operand::Stack(rhs),
            } => {
                new_instructions.push(Instruction::Mov {
                    src: Operand::Register(Register::CX),
                    dst: Operand::Register(Register::R10),
                });
                new_instructions.push(Instruction::Mov {
                    src: Operand::Stack(lhs),
                    dst: Operand::Register(Register::CX),
                });
                new_instructions.push(Instruction::Binary {
                    op: BinaryOperator::Sal,
                    lhs: Operand::Register(Register::CX),
                    rhs: Operand::Stack(rhs),
                });
                new_instructions.push(Instruction::Mov {
                    dst: Operand::Register(Register::CX),
                    src: Operand::Register(Register::R10),
                });
            }
            Instruction::Binary {
                op: BinaryOperator::Sar,
                lhs: Operand::Stack(lhs),
                rhs: Operand::Stack(rhs),
            } => {
                new_instructions.push(Instruction::Mov {
                    src: Operand::Register(Register::CX),
                    dst: Operand::Register(Register::R10),
                });
                new_instructions.push(Instruction::Mov {
                    src: Operand::Stack(lhs),
                    dst: Operand::Register(Register::CX),
                });
                new_instructions.push(Instruction::Binary {
                    op: BinaryOperator::Sar,
                    lhs: Operand::Register(Register::CX),
                    rhs: Operand::Stack(rhs),
                });
                new_instructions.push(Instruction::Mov {
                    dst: Operand::Register(Register::CX),
                    src: Operand::Register(Register::R10),
                });
            }
            Instruction::Binary {
                op,
                lhs: Operand::Stack(lhs),
                rhs: Operand::Stack(rhs),
            } => {
                new_instructions.push(Instruction::Mov {
                    src: Operand::Stack(lhs),
                    dst: Operand::Register(Register::R10),
                });
                new_instructions.push(Instruction::Binary {
                    op,
                    lhs: Operand::Register(Register::R10),
                    rhs: Operand::Stack(rhs),
                });
            }
            Instruction::Idiv(Operand::Imm(val)) => {
                new_instructions.push(Instruction::Mov {
                    src: Operand::Imm(val),
                    dst: Operand::Register(Register::R10),
                });
                new_instructions.push(Instruction::Idiv(Operand::Register(Register::R10)));
            }
            Instruction::Cmp {
                lhs: left @ Operand::Stack(_),
                rhs: right @ Operand::Stack(_),
            } => {
                new_instructions.push(Instruction::Mov {
                    src: left,
                    dst: Operand::Register(Register::R10),
                });
                new_instructions.push(Instruction::Cmp {
                    lhs: Operand::Register(Register::R10),
                    rhs: right,
                });
            }
            Instruction::Cmp {
                lhs: left,
                rhs: right @ Operand::Imm(_),
            } => {
                new_instructions.push(Instruction::Mov {
                    src: right,
                    dst: Operand::Register(Register::R11),
                });
                new_instructions.push(Instruction::Cmp {
                    lhs: left,
                    rhs: Operand::Register(Register::R11),
                });
            }
            _ => {
                new_instructions.push(inst);
            }
        }
    }

    new_instructions
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_multiply() {
        let input_instructions = vec![Instruction::Binary {
            op: BinaryOperator::Mult,
            lhs: Operand::Stack(4),
            rhs: Operand::Stack(8),
        }];
        let expected_instructions = vec![
            Instruction::Mov {
                src: Operand::Stack(8),
                dst: Operand::Register(Register::R11),
            },
            Instruction::Binary {
                op: BinaryOperator::Mult,
                lhs: Operand::Stack(4),
                rhs: Operand::Register(Register::R11),
            },
            Instruction::Mov {
                src: Operand::Register(Register::R11),
                dst: Operand::Stack(8),
            },
        ];
        let output = fixup_instructions(input_instructions);

        assert_eq!(expected_instructions.len(), output.len());

        for (expected, received) in expected_instructions.into_iter().zip(output) {
            assert_eq!(expected, received);
        }
    }
}
