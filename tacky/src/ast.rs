use std::fmt::Display;

use rrbc_parser::ast;

use crate::{BinaryOperator, UnaryOperator};

/// A ast::AssignmentOperator::None cannot be turned into a BinaryOperator
#[derive(Debug)]
pub struct InvalidAssignmentOperator();

impl Display for InvalidAssignmentOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Cannot turn a AssignmentOperator without an actual BinaryOperator into one."
        )
    }
}

impl TryFrom<ast::AssignmentOperator> for BinaryOperator {
    type Error = InvalidAssignmentOperator;

    /// Errors if the value is a AssignmentOperator::None
    fn try_from(value: ast::AssignmentOperator) -> Result<Self, Self::Error> {
        Ok(match value {
            ast::AssignmentOperator::None => return Err(InvalidAssignmentOperator()),
            ast::AssignmentOperator::Add => BinaryOperator::Add,
            ast::AssignmentOperator::Subtract => BinaryOperator::Subtract,
            ast::AssignmentOperator::Multiply => BinaryOperator::Multiply,
            ast::AssignmentOperator::Divide => BinaryOperator::Divide,
            ast::AssignmentOperator::Reminder => BinaryOperator::Remainder,
            ast::AssignmentOperator::BitwiseOr => BinaryOperator::BitwiseOr,
            ast::AssignmentOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
            ast::AssignmentOperator::BitwiseXor => BinaryOperator::Xor,
            ast::AssignmentOperator::ShiftLeft => BinaryOperator::Sal,
            ast::AssignmentOperator::ShiftRight => BinaryOperator::Sar,
        })
    }
}

/// Logical ast::BinaryOperator cannot be turned into a BinaryOperator
/// 0 contains the problematic Operator, if you want it back ;-)
#[derive(Debug)]
pub struct InvalidBinaryOperator(pub ast::BinaryOperator);

impl Display for InvalidBinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Logical operators cannot be turned into a BinaryOperator"
        )
    }
}

impl TryFrom<ast::BinaryOperator> for BinaryOperator {
    type Error = InvalidBinaryOperator;

    fn try_from(value: ast::BinaryOperator) -> Result<Self, Self::Error> {
        Ok(match value {
            ast::BinaryOperator::Add => BinaryOperator::Add,
            ast::BinaryOperator::Subtract => BinaryOperator::Subtract,
            ast::BinaryOperator::Multiply => BinaryOperator::Multiply,
            ast::BinaryOperator::Divide => BinaryOperator::Divide,
            ast::BinaryOperator::Reminder => BinaryOperator::Remainder,
            ast::BinaryOperator::ShiftLeft => BinaryOperator::Sal,
            ast::BinaryOperator::ShiftRight => BinaryOperator::Sar,
            ast::BinaryOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
            ast::BinaryOperator::Xor => BinaryOperator::Xor,
            ast::BinaryOperator::BitwiseOr => BinaryOperator::BitwiseOr,
            ast::BinaryOperator::And => return Err(InvalidBinaryOperator(value)),
            ast::BinaryOperator::Or => return Err(InvalidBinaryOperator(value)),
            ast::BinaryOperator::Equal => BinaryOperator::Equal,
            ast::BinaryOperator::NotEqual => BinaryOperator::NotEqual,
            ast::BinaryOperator::LessThan => BinaryOperator::LessThan,
            ast::BinaryOperator::LessOrEqual => BinaryOperator::LessOrEqual,
            ast::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
            ast::BinaryOperator::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
        })
    }
}

#[derive(Debug)]
pub struct InvalidUnaryOperator(pub ast::UnaryOperator);

impl Display for InvalidUnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Increment and Decrement Unary Operators cannot be turned into a UnaryOperator, they are just syntactical sugar"
        )
    }
}

impl TryFrom<ast::UnaryOperator> for UnaryOperator {
    type Error = InvalidUnaryOperator;

    fn try_from(value: ast::UnaryOperator) -> Result<Self, Self::Error> {
        Ok(match value {
            ast::UnaryOperator::Complement => UnaryOperator::Complement,
            ast::UnaryOperator::Negate => UnaryOperator::Negate,
            ast::UnaryOperator::Not => UnaryOperator::Not,
            ast::UnaryOperator::Increment | ast::UnaryOperator::Decrement => {
                return Err(InvalidUnaryOperator(value))
            }
        })
    }
}
