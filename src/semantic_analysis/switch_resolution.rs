use thiserror::Error;

use crate::ast::{Block, BlockItem, FunctionDefinition, Program, Statement};

#[derive(Debug, Error)]
pub enum SwitchResolutionError {
    #[error("Default label is not in a switch statement.")]
    DefaultNotInSwitch,
    #[error("Case label is not in a switch statement.")]
    CaseNotInSwitch,
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
    let body = resolve_block(function.body, None)?;
    Ok(FunctionDefinition {
        name: function.name,
        body,
    })
}

fn resolve_block(
    block: Block,
    current_switch: Option<String>,
) -> Result<Block, SwitchResolutionError> {
    let mut items = vec![];

    for item in block.0 {
        items.push(resolve_block_item(item, current_switch.clone())?);
    }

    Ok(Block(items))
}

fn resolve_block_item(
    item: BlockItem,
    current_switch: Option<String>,
) -> Result<BlockItem, SwitchResolutionError> {
    match item {
        BlockItem::S(stmt) => Ok(BlockItem::S(resolve_statement(stmt, current_switch)?)),
        BlockItem::D(decl) => Ok(BlockItem::D(decl)),
    }
}

fn resolve_statement(
    stmt: Statement,
    current_switch: Option<String>,
) -> Result<Statement, SwitchResolutionError> {
    Ok(match stmt {
        Statement::If {
            condition,
            then,
            r#else,
        } => Statement::If {
            condition,
            then: Box::new(resolve_statement(*then, current_switch.clone())?),
            r#else: match r#else {
                Some(stmt) => Some(Box::new(resolve_statement(*stmt, current_switch)?)),
                None => None,
            },
        },
        Statement::Label(label, stmt) => {
            Statement::Label(label, Box::new(resolve_statement(*stmt, current_switch)?))
        }
        Statement::Default(stmt, _) => match current_switch {
            None => return Err(SwitchResolutionError::DefaultNotInSwitch),
            Some(label) => Statement::Default(stmt, label),
        },
        Statement::Case(_, _, _) => todo!(),
        Statement::Compound(_) => todo!(),
        Statement::Break(_) => todo!(),
        Statement::While {
            condition,
            body,
            label,
        } => todo!(),
        Statement::DoWhile {
            body,
            condition,
            label,
        } => todo!(),
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => todo!(),
        Statement::Switch {
            expression,
            body,
            data,
        } => todo!(),

        stmt @ (Statement::Return(_)
        | Statement::Null
        | Statement::Expression(_)
        | Statement::Goto(_)
        | Statement::Continue(_)) => stmt,
    })
}
