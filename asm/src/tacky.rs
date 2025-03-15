use std::fmt::Display;

use crate::{Operand, UnaryOperator};

impl From<rrbc_tacky::Value> for Operand {
    fn from(value: rrbc_tacky::Value) -> Self {
        match value {
            rrbc_tacky::Value::Constant(val) => Operand::Imm(val),
            rrbc_tacky::Value::Var(var) => Operand::Pseudo(var.0),
        }
    }
}

impl From<&rrbc_tacky::Value> for Operand {
    fn from(value: &rrbc_tacky::Value) -> Self {
        match value.clone() {
            rrbc_tacky::Value::Constant(val) => Operand::Imm(val),
            rrbc_tacky::Value::Var(var) => Operand::Pseudo(var.0),
        }
    }
}

#[derive(Debug)]
pub struct InvalidUnaryOperator(pub rrbc_tacky::UnaryOperator);

impl Display for InvalidUnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Not Unary Operator cannot be turned into a UnaryOperator, it is just syntactical sugar"
        )
    }
}

impl TryFrom<rrbc_tacky::UnaryOperator> for UnaryOperator {
    type Error = InvalidUnaryOperator;

    fn try_from(value: rrbc_tacky::UnaryOperator) -> Result<Self, Self::Error> {
        Ok(match value {
            rrbc_tacky::UnaryOperator::Complement => UnaryOperator::Not,
            rrbc_tacky::UnaryOperator::Negate => UnaryOperator::Neg,
            v => return Err(InvalidUnaryOperator(v)),
        })
    }
}
