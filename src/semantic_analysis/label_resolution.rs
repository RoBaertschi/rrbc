use std::collections::HashMap;

use thiserror::Error;

use crate::{
    ast::{Block, BlockItem, FunctionDefinition, Program, Statement},
    unique_id,
};

type LabelMap = HashMap<String, String>;

struct State {
    map: LabelMap,
    function_name: String,
}

#[derive(Error, Debug)]
pub enum LabelResolutionError {
    #[error("Reused label \"{0}\"")]
    LabelReused(String),
    #[error("Could not find label \"{0}\"")]
    LabelNotFound(String),
}

pub fn resolve_program(program: Program) -> Result<Program, LabelResolutionError> {
    Ok(Program {
        function_definition: resolve_function(program.function_definition)?,
    })
}

pub fn resolve_function(
    function: FunctionDefinition,
) -> Result<FunctionDefinition, LabelResolutionError> {
    let mut state = State {
        map: LabelMap::new(),
        function_name: function.name,
    };

    let mut body = find_label_resolve_block(&mut state, function.body)?;
    body = resolve_block(&mut state, body)?;

    Ok(FunctionDefinition {
        name: state.function_name,
        body,
    })
}

fn resolve_block(state: &mut State, block: Block) -> Result<Block, LabelResolutionError> {
    let mut blocks = vec![];
    for item in block.0 {
        blocks.push(resolve_block_item(state, item)?);
    }
    Ok(Block(blocks))
}

fn find_label_resolve_block(
    state: &mut State,
    block: Block,
) -> Result<Block, LabelResolutionError> {
    let mut blocks = vec![];
    for item in block.0 {
        blocks.push(find_label_resolve_block_item(state, item)?);
    }
    Ok(Block(blocks))
}

fn find_label_resolve_block_item(
    state: &mut State,
    item: BlockItem,
) -> Result<BlockItem, LabelResolutionError> {
    match item {
        BlockItem::S(stmt) => Ok(BlockItem::S(find_label_resolve_statement(state, stmt)?)),
        item => Ok(item),
    }
}

fn find_label_resolve_statement(
    state: &mut State,
    stmt: Statement,
) -> Result<Statement, LabelResolutionError> {
    match stmt {
        Statement::Label(name, statement) => {
            if state.map.contains_key(&name) {
                return Err(LabelResolutionError::LabelReused(name));
            }

            let unique_name = unique_id::temp_c_label_name(&name, &state.function_name);
            state.map.insert(name, unique_name.clone());

            Ok(Statement::Label(
                unique_name,
                Box::new(find_label_resolve_statement(state, *statement)?),
            ))
        }
        Statement::Return(expr) => Ok(Statement::Return(expr)),
        Statement::Expression(expr) => Ok(Statement::Expression(expr)),
        Statement::If {
            condition,
            then,
            r#else,
        } => Ok(Statement::If {
            condition,
            then: Box::new(find_label_resolve_statement(state, *then)?),
            r#else: match r#else {
                Some(stmt) => Some(Box::new(find_label_resolve_statement(state, *stmt)?)),
                None => None,
            },
        }),
        Statement::Null => Ok(Statement::Null),
        // Later
        Statement::Goto(name) => Ok(Statement::Goto(name)),
        Statement::Compound(block) => {
            Ok(Statement::Compound(find_label_resolve_block(state, block)?))
        }
    }
}

fn resolve_block_item(
    state: &mut State,
    item: BlockItem,
) -> Result<BlockItem, LabelResolutionError> {
    match item {
        BlockItem::S(stmt) => Ok(BlockItem::S(resolve_statement(state, stmt)?)),
        item => Ok(item),
    }
}

fn resolve_statement(
    state: &mut State,
    stmt: Statement,
) -> Result<Statement, LabelResolutionError> {
    match stmt {
        Statement::Goto(name) => {
            if !state.map.contains_key(&name) {
                return Err(LabelResolutionError::LabelNotFound(name));
            }

            Ok(Statement::Goto(
                state
                    .map
                    .get(&name)
                    .ok_or(LabelResolutionError::LabelNotFound(name.clone()))?
                    .clone(),
            ))
        }
        Statement::Return(expr) => Ok(Statement::Return(expr)),
        Statement::Expression(expr) => Ok(Statement::Expression(expr)),
        Statement::If {
            condition,
            then,
            r#else,
        } => Ok(Statement::If {
            condition,
            then: Box::new(resolve_statement(state, *then)?),
            r#else: match r#else {
                Some(stmt) => Some(Box::new(resolve_statement(state, *stmt)?)),
                None => None,
            },
        }),
        Statement::Null => Ok(Statement::Null),
        Statement::Label(name, stmt) => Ok(Statement::Label(
            name,
            Box::new(resolve_statement(state, *stmt)?),
        )),
        Statement::Compound(block) => Ok(Statement::Compound(resolve_block(state, block)?)),
    }
}
