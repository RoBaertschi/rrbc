use std::collections::HashMap;

use crate::assembly::{self, FunctionDefinition, Instruction, Program};

pub fn run_second_pass(program: Program) -> (Program, u32) {
    SecondPass {
        offest_to_identifier: HashMap::new(),
        stack_offset: 0,
    }
    .gen(program)
}

struct SecondPass {
    offest_to_identifier: HashMap<String, u32>,
    stack_offset: u32,
}

impl SecondPass {
    pub fn gen(&mut self, program: assembly::Program) -> (Program, u32) {
        (Program(self.function(program.0)), self.stack_offset)
    }

    fn function(&mut self, func: assembly::FunctionDefinition) -> assembly::FunctionDefinition {
        FunctionDefinition {
            name: func.name,
            instructions: self.instructions(func.instructions),
        }
    }

    fn instructions(
        &mut self,
        mut instructions: Vec<assembly::Instruction>,
    ) -> Vec<assembly::Instruction> {
        for inst in instructions.iter_mut() {
            match inst {
                Instruction::Ret => {}
                Instruction::Mov { src, dst } => {
                    self.pseudo_to_stack(src);
                    self.pseudo_to_stack(dst);
                }
                Instruction::Unary { op: _, operand } => self.pseudo_to_stack(operand),
                Instruction::Binary {
                    op: _,
                    lhs: op1,
                    rhs: op2,
                } => {
                    self.pseudo_to_stack(op1);
                    self.pseudo_to_stack(op2);
                }
                Instruction::Idiv(operand) => self.pseudo_to_stack(operand),
                Instruction::Cdq | Instruction::AllocateStack(_) => {}
            }
        }

        instructions
    }

    fn pseudo_to_stack(&mut self, operand: &mut assembly::Operand) {
        match operand {
            assembly::Operand::Pseudo(ident) => {
                *operand = assembly::Operand::Stack(
                    *self
                        .offest_to_identifier
                        .entry(ident.to_owned())
                        .or_insert_with(|| {
                            self.stack_offset += 4;
                            self.stack_offset
                        }),
                )
            }
            _ => {}
        }
    }
}
