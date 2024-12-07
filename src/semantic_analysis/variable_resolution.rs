//! TODO: Move this into the Parser for support with typedefs someday.

use std::collections::HashMap;

use thiserror::Error;

use crate::{
    ast::{Block, BlockItem, Declaration, Expression, ForInit, Program, Statement, UnaryOperator},
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

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
struct VariableMapEntry {
    new_name: String,
    from_current_block: bool,
}
type VariableMap = HashMap<String, VariableMapEntry>;

fn copy_variable_map(variable_map: &VariableMap) -> VariableMap {
    // Expensive but necessary
    let mut new_variable_map = variable_map.clone();
    for map_entry in new_variable_map.iter_mut() {
        map_entry.1.from_current_block = false;
    }
    new_variable_map
}

pub fn resolve_program(program: Program) -> Result<Program, VariableResolutionError> {
    let mut variable_map = VariableMap::new();

    Ok(Program {
        function_definition: crate::ast::FunctionDefinition {
            name: program.function_definition.name,
            body: resolve_block(&mut variable_map, program.function_definition.body)?,
        },
    })
}

fn resolve_block(
    variable_map: &mut VariableMap,
    block: Block,
) -> Result<Block, VariableResolutionError> {
    let mut blocks = vec![];
    for item in block.0 {
        blocks.push(resolve_block_item(variable_map, item)?);
    }

    Ok(Block(blocks))
}

fn resolve_block_item(
    variable_map: &mut VariableMap,
    block_item: BlockItem,
) -> Result<BlockItem, VariableResolutionError> {
    Ok(match block_item {
        BlockItem::S(s) => BlockItem::S(resolve_statement(variable_map, s)?),
        BlockItem::D(d) => BlockItem::D(resolve_declaration(variable_map, d)?),
    })
}

fn resolve_declaration(
    variable_map: &mut VariableMap,
    decl: Declaration,
) -> Result<Declaration, VariableResolutionError> {
    if let Some(entry) = variable_map.get(&decl.name) {
        if entry.from_current_block {
            return Err(VariableResolutionError::VariableNameRedeclaration(
                decl.name,
            ));
        }
    }

    let unique_name = unique_id::temp_c_variable_name(&decl.name);
    variable_map.insert(
        decl.name,
        VariableMapEntry {
            new_name: unique_name.clone(),
            from_current_block: true,
        },
    );

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

fn resolve_statement(
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
        Statement::Compound(block) => {
            let mut new_variable_map = copy_variable_map(variable_map);
            Statement::Compound(resolve_block(&mut new_variable_map, block)?)
        }
        Statement::Break(label) => Statement::Break(label),
        Statement::Continue(label) => Statement::Continue(label),
        Statement::While {
            condition,
            body,
            label,
        } => Statement::While {
            condition: resolve_expression(variable_map, condition)?,
            body: Box::new(resolve_statement(variable_map, *body)?),
            label,
        },
        Statement::DoWhile {
            body,
            condition,
            label,
        } => Statement::DoWhile {
            condition: resolve_expression(variable_map, condition)?,
            body: Box::new(resolve_statement(variable_map, *body)?),
            label,
        },
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let mut new_variable_map = copy_variable_map(variable_map);
            Statement::For {
                init: resolve_for_init(&mut new_variable_map, init)?,
                condition: resolve_optional_expression(&mut new_variable_map, condition)?,
                post: resolve_optional_expression(&mut new_variable_map, post)?,
                body: Box::new(resolve_statement(&mut new_variable_map, *body)?),
                label,
            }
        }
    })
}

fn resolve_optional_expression(
    variable_map: &mut VariableMap,
    expr: Option<Expression>,
) -> Result<Option<Expression>, VariableResolutionError> {
    match expr {
        Some(expr) => Ok(Some(resolve_expression(variable_map, expr)?)),
        None => Ok(None),
    }
}

fn resolve_for_init(
    variable_map: &mut VariableMap,
    init: ForInit,
) -> Result<ForInit, VariableResolutionError> {
    match init {
        ForInit::InitDecl(declaration) => Ok(ForInit::InitDecl(resolve_declaration(
            variable_map,
            declaration,
        )?)),
        ForInit::InitExp(expression) => Ok(ForInit::InitExp(resolve_expression(
            variable_map,
            expression,
        )?)),
        ForInit::None => Ok(ForInit::None),
    }
}

fn resolve_variable(
    variable_map: &mut VariableMap,
    expr: Expression,
) -> Result<Expression, VariableResolutionError> {
    match expr {
        Expression::Var(name) => Ok(Expression::Var(
            variable_map
                .get(&name)
                .ok_or(VariableResolutionError::VariableNameNotFound(name))?
                .new_name
                .clone(),
        )),

        expr => Err(VariableResolutionError::InvalidLvalue(expr)),
    }
}

fn resolve_expression(
    variable_map: &mut VariableMap,
    expr: Expression,
) -> Result<Expression, VariableResolutionError> {
    Ok(match expr {
        Expression::Var(name) => match variable_map.get(&name) {
            Some(entry) => Expression::Var(entry.new_name.clone()),
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
