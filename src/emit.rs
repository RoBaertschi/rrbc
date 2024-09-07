use std::env::consts::OS;

use crate::assembly::{FunctionDefinition, Instruction, Operand, Program};

/// A Structure that implements this trait, can emit assembly using the provided function.
/// A high-level construct should also add a comment with more debugging information.
/// A high-level construct would be a function, statement or expression.
pub trait EmitAsm {
    /// The indent_depth argument only needs to be used when you have to indent something.
    fn emit(&self, indent_depth: u32) -> String;
}

impl EmitAsm for Operand {
    fn emit(&self, _: u32) -> String {
        match self {
            Operand::Register(reg) => {
                "%".to_owned()
                    + match reg {
                        crate::assembly::Register::EAX => "eax",
                    }
            }
            Operand::Imm(val) => format!("${}", val),
        }
    }
}

impl EmitAsm for Instruction {
    fn emit(&self, indent_depth: u32) -> String {
        let tabs = "\t".repeat(indent_depth as usize);

        match self {
            Instruction::Mov { src, dst } => format!(
                "{}movl {}, {}\n",
                tabs,
                src.emit(indent_depth),
                dst.emit(indent_depth),
            ),
            Instruction::Ret() => format!("{}ret\n", tabs),
        }
    }
}

impl EmitAsm for FunctionDefinition {
    fn emit(&self, indent_depth: u32) -> String {
        let tabs = "\t".repeat((indent_depth + 1) as usize);

        format!(
            "# {:?}\n{}\n{}:\n{}\n# End function\n",
            self,
            format!("{}.global {}", tabs, self.name),
            if OS == "macos" {
                format!("_{}", self.name)
            } else {
                self.name.to_owned()
            },
            self.instructions
                .iter()
                .map(|inst| inst.emit(indent_depth + 1))
                .reduce(|acc, inst| format!("{}{}", acc, inst))
                .unwrap_or("".to_owned())
        )
    }
}

impl EmitAsm for Program {
    fn emit(&self, indent_depth: u32) -> String {
        format!(
            "{}\n{}",
            self.0.emit(indent_depth),
            if OS == "linux" {
                ".section .note.GNU-stack,\"\",@progbits\n"
            } else {
                ""
            }
        )
    }
}
