//! TODO: Move this into the Parser for support with typedefs someday.

use std::collections::HashMap;

use thiserror::Error;

use crate::{
    ast::{BlockItem, Declaration, Expression, Program, Statement, UnaryOperator},
    unique_id,
};

#[derive(Error, Debug)]
pub enum VariableResolutionError {
    #[error("Variable {0} was redeclared in the same scope")]
    VariableNameRedeclaration(String),
    #[error("Variable {0} was not declared in this scope")]
    VariableNameNotFound(String),
    #[error("Invalid assignment on non lvalue {0:?}")]
    InvalidLvalue(Expression),
}

type VariableMap = HashMap<String, String>;

pub fn resolve_program(program: Program) -> Result<Program, VariableResolutionError> {
    let mut map = VariableMap::new();

    let mut blocks = vec![];
    for item in program.function_definition.body {
        blocks.push(resolve_block_item(&mut map, item)?);
    }

    Ok(Program {
        function_definition: crate::ast::FunctionDefinition {
            name: program.function_definition.name,
            body: blocks,
        },
    })
}

pub fn resolve_block_item(
    variable_map: &mut VariableMap,
    block_item: BlockItem,
) -> Result<BlockItem, VariableResolutionError> {
    Ok(match block_item {
        BlockItem::S(s) => BlockItem::S(resolve_statement(variable_map, s)?),
        BlockItem::D(d) => BlockItem::D(resolve_declaration(variable_map, d)?),
    })
}

pub fn resolve_declaration(
    variable_map: &mut VariableMap,
    decl: Declaration,
) -> Result<Declaration, VariableResolutionError> {
    if variable_map.contains_key(&decl.name) {
        return Err(VariableResolutionError::VariableNameRedeclaration(
            decl.name,
        ));
    }

    let unique_name = unique_id::temp_c_variable_name(&decl.name);
    variable_map.insert(decl.name, unique_name.clone());

    let expr = if let Some(expr) = decl.exp {
        Some(resolve_expression(variable_map, expr)?)
    } else {
        None
    };

    Ok(Declaration {
        name: unique_name,
        exp: expr,
    })
}

pub fn resolve_statement(
    variable_map: &mut VariableMap,
    stmt: Statement,
) -> Result<Statement, VariableResolutionError> {
    Ok(match stmt {
        Statement::Return(expr) => Statement::Return(resolve_expression(variable_map, expr)?),
        Statement::Expression(expr) => {
            Statement::Expression(resolve_expression(variable_map, expr)?)
        }
        Statement::Null => Statement::Null,
        Statement::If {
            condition,
            then,
            r#else,
        } => {
            let resolved_else = match r#else {
                Some(r#else) => Some(Box::new(resolve_statement(variable_map, *r#else)?)),
                None => None,
            };
            Statement::If {
                condition: resolve_expression(variable_map, condition)?,
                then: Box::new(resolve_statement(variable_map, *then)?),
                r#else: resolved_else,
            }
        }
        Statement::Goto(ident) => Statement::Goto(ident),
        Statement::Label(ident, stmt) => {
            Statement::Label(ident, Box::new(resolve_statement(variable_map, *stmt)?))
        }
    })
}

pub fn resolve_variable(
    variable_map: &mut VariableMap,
    expr: Expression,
) -> Result<Expression, VariableResolutionError> {
    match expr {
        Expression::Var(name) => Ok(Expression::Var(
            variable_map
                .get(&name)
                .ok_or(VariableResolutionError::VariableNameNotFound(name))?
                .clone(),
        )),

        expr => Err(VariableResolutionError::InvalidLvalue(expr)),
    }
}

pub fn resolve_expression(
    variable_map: &mut VariableMap,
    expr: Expression,
) -> Result<Expression, VariableResolutionError> {
    Ok(match expr {
        Expression::Var(name) => match variable_map.get(&name) {
            Some(unique_name) => Expression::Var(unique_name.clone()),
            None => return Err(VariableResolutionError::VariableNameNotFound(name)),
        },
        Expression::Assignment { lhs, rhs, op } => Expression::Assignment {
            op,
            lhs: Box::new(resolve_variable(variable_map, *lhs)?),
            rhs: Box::new(resolve_expression(variable_map, *rhs)?),
        },
        Expression::Unary {
            op: op @ (UnaryOperator::Decrement | UnaryOperator::Increment),
            expression: expr,
        } => Expression::Unary {
            op,
            expression: Box::new(resolve_variable(variable_map, *expr)?),
        },
        Expression::Postfix(postfix_operator, expression) => Expression::Postfix(
            postfix_operator,
            Box::new(resolve_variable(variable_map, *expression)?),
        ),
        Expression::Unary {
            op,
            expression: expr,
        } => Expression::Unary {
            op,
            expression: Box::new(resolve_expression(variable_map, *expr)?),
        },
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op,
            lhs: Box::new(resolve_expression(variable_map, *lhs)?),
            rhs: Box::new(resolve_expression(variable_map, *rhs)?),
        },
        Expression::Constant(constant) => Expression::Constant(constant),
        Expression::Conditional {
            condition,
            then,
            r#else,
        } => Expression::Conditional {
            condition: Box::new(resolve_expression(variable_map, *condition)?),
            then: Box::new(resolve_expression(variable_map, *then)?),
            r#else: Box::new(resolve_expression(variable_map, *r#else)?),
        },
    })
}
