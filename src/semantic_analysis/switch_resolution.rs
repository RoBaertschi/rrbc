use std::collections::HashMap;

use thiserror::Error;

use crate::ast::{Block, BlockItem, Expression, FunctionDefinition, Program, Statement};

#[derive(Debug, Error)]
pub enum SwitchResolutionError {
    #[error("Default label is not in a switch statement.")]
    DefaultNotInSwitch,
    #[error("Case label is not in a switch statement.")]
    CaseNotInSwitch,
    #[error("Case expression is not compiletime constant {0:?}")]
    ExpressionIsNotConstant(Expression),
}

struct Data {
    case_map: HashMap<Option<i32>, String>,
    current_switch: Option<String>,
}

pub fn resolve_program(program: Program) -> Result<Program, SwitchResolutionError> {
    let function_definition = resolve_function_definition(program.function_definition)?;
    Ok(Program {
        function_definition,
    })
}

fn resolve_function_definition(
    function: FunctionDefinition,
) -> Result<FunctionDefinition, SwitchResolutionError> {
    let mut data = Data {
        case_map: HashMap::new(),
        current_switch: None,
    };

    let body = resolve_block(function.body, &mut data)?;
    Ok(FunctionDefinition {
        name: function.name,
        body,
    })
}

fn resolve_block(block: Block, data: &mut Data) -> Result<Block, SwitchResolutionError> {
    let mut items = vec![];

    for item in block.0 {
        items.push(resolve_block_item(item, data)?);
    }

    Ok(Block(items))
}

fn resolve_block_item(
    item: BlockItem,
    data: &mut Data,
) -> Result<BlockItem, SwitchResolutionError> {
    match item {
        BlockItem::S(stmt) => Ok(BlockItem::S(resolve_statement(stmt, data)?)),
        BlockItem::D(decl) => Ok(BlockItem::D(decl)),
    }
}

fn resolve_constant(expression: Expression) -> Result<i32, SwitchResolutionError> {
    match expression {
        Expression::Constant(c) => Ok(c),
        expr => Err(SwitchResolutionError::ExpressionIsNotConstant(expr)),
    }
}

fn resolve_statement(stmt: Statement, data: &mut Data) -> Result<Statement, SwitchResolutionError> {
    Ok(match stmt {
        Statement::If {
            condition,
            then,
            r#else,
        } => Statement::If {
            condition,
            then: Box::new(resolve_statement(*then, data)?),
            r#else: match r#else {
                Some(stmt) => Some(Box::new(resolve_statement(*stmt, data)?)),
                None => None,
            },
        },
        Statement::Label(label, stmt) => {
            Statement::Label(label, Box::new(resolve_statement(*stmt, data)?))
        }
        Statement::Default(stmt, _) => match data.current_switch {
            None => return Err(SwitchResolutionError::DefaultNotInSwitch),
            Some(ref label) => Statement::Default(stmt, label.clone()),
        },
        Statement::Case(expr, stmt, _) => match data.current_switch {
            None => return Err(SwitchResolutionError::CaseNotInSwitch),
            Some(ref label) => Statement::Case(expr, stmt, label.clone()),
        },
        Statement::Compound(block) => Statement::Compound(resolve_block(block, data)?),
        Statement::While {
            condition,
            body,
            label,
        } => Statement::While {
            condition,
            body: Box::new(resolve_statement(*body, data)?),
            label,
        },
        Statement::DoWhile {
            body,
            condition,
            label,
        } => Statement::DoWhile {
            condition,
            body: Box::new(resolve_statement(*body, data)?),
            label,
        },
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => Statement::For {
            init,
            condition,
            post,
            body: Box::new(resolve_statement(*body, data)?),
            label,
        },
        Statement::Switch {
            expression,
            body,
            data,
            label,
        } => todo!(),

        stmt @ (Statement::Return(_)
        | Statement::Null
        | Statement::Expression(_)
        | Statement::Goto(_)
        | Statement::Continue(_)
        | Statement::Break(_)) => stmt,
    })
}
