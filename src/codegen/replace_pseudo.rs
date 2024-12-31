use std::collections::HashMap;

use crate::assembly::{self, FunctionDefinition, Instruction, Program};

pub fn run_second_pass(program: Program) -> (Program, HashMap<String, usize>) {
    SecondPass {
        offest_to_identifier: HashMap::new(),
        stack_offset: HashMap::new(),
        current_function: String::new(),
    }
    .gen(program)
}

struct SecondPass {
    offest_to_identifier: HashMap<String, usize>,
    stack_offset: HashMap<String, usize>,
    current_function: String,
}

impl SecondPass {
    pub fn gen(mut self, program: assembly::Program) -> (Program, HashMap<String, usize>) {
        (
            Program(
                program
                    .0
                    .into_iter()
                    .map(|func| self.function(func))
                    .collect(),
            ),
            self.stack_offset,
        )
    }

    fn function(&mut self, func: assembly::FunctionDefinition) -> assembly::FunctionDefinition {
        self.current_function = func.name.clone();
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
                Instruction::Push(operand) => self.pseudo_to_stack(operand),
                Instruction::Cdq
                | Instruction::AllocateStack(_)
                | Instruction::Jmp(_)
                | Instruction::JumpCC(_, _)
                | Instruction::Label(_)
                | Instruction::DeallocateStack(_)
                | Instruction::Call(_) => {}
                Instruction::Cmp { lhs, rhs } => {
                    self.pseudo_to_stack(lhs);
                    self.pseudo_to_stack(rhs);
                }
                Instruction::SetCC(_, operand) => {
                    self.pseudo_to_stack(operand);
                }
            }
        }

        instructions
    }

    fn pseudo_to_stack(&mut self, operand: &mut assembly::Operand) {
        if let assembly::Operand::Pseudo(ident) = operand {
            *operand = assembly::Operand::Stack(
                *self
                    .offest_to_identifier
                    .entry(ident.clone())
                    .or_insert_with(|| {
                        let stack_offset = self
                            .stack_offset
                            .entry(self.current_function.clone())
                            .or_insert(0_usize);
                        *stack_offset += 4;
                        *stack_offset
                    }),
            )
        }
    }
}
