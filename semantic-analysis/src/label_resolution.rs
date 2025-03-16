use std::collections::HashMap;

use thiserror::Error;

use rrbc_parser::ast::{Block, BlockItem, FunctionDeclaration, Program, Statement};
use rrbc_utils::unique_id;

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
    let mut new_funcs = vec![];

    for func in program.declarations {
        new_funcs.push(resolve_function(func)?);
    }

    Ok(Program {
        declarations: new_funcs,
    })
}

pub fn resolve_function(
    function: FunctionDeclaration,
) -> Result<FunctionDeclaration, LabelResolutionError> {
    let mut state = State {
        map: LabelMap::new(),
        function_name: function.name,
    };

    let body = match function.body {
        Some(body) => Some(
            find_label_resolve_block(&mut state, body)
                .and_then(|ok| resolve_block(&mut state, ok))?,
        ),
        None => None,
    };

    Ok(FunctionDeclaration {
        name: state.function_name,
        params: function.params,
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
        Statement::Break(label) => Ok(Statement::Break(label)),
        Statement::Continue(label) => Ok(Statement::Continue(label)),
        Statement::While {
            condition,
            body,
            label,
        } => Ok(Statement::While {
            condition,
            body: Box::new(find_label_resolve_statement(state, *body)?),
            label,
        }),
        Statement::DoWhile {
            body,
            condition,
            label,
        } => Ok(Statement::DoWhile {
            condition,
            body: Box::new(find_label_resolve_statement(state, *body)?),
            label,
        }),
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => Ok(Statement::For {
            init,
            condition,
            post,
            body: Box::new(find_label_resolve_statement(state, *body)?),
            label,
        }),
        Statement::Default(statement, label) => Ok(Statement::Default(
            Box::new(find_label_resolve_statement(state, *statement)?),
            label,
        )),
        Statement::Case(expression, statement, label) => Ok(Statement::Case(
            expression,
            Box::new(find_label_resolve_statement(state, *statement)?),
            label,
        )),
        Statement::Switch {
            expression,
            body,
            label,
            cases,
        } => Ok(Statement::Switch {
            expression,
            body: Box::new(find_label_resolve_statement(state, *body)?),
            label,
            cases,
        }),
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
        Statement::Break(label) => Ok(Statement::Break(label)),
        Statement::Continue(label) => Ok(Statement::Continue(label)),
        Statement::While {
            condition,
            body,
            label,
        } => Ok(Statement::While {
            condition,
            body: Box::new(resolve_statement(state, *body)?),
            label,
        }),
        Statement::DoWhile {
            body,
            condition,
            label,
        } => Ok(Statement::DoWhile {
            condition,
            body: Box::new(resolve_statement(state, *body)?),
            label,
        }),
        Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => Ok(Statement::For {
            init,
            condition,
            post,
            body: Box::new(resolve_statement(state, *body)?),
            label,
        }),
        Statement::Default(statement, label) => Ok(Statement::Default(
            Box::new(resolve_statement(state, *statement)?),
            label,
        )),
        Statement::Case(expression, statement, label) => Ok(Statement::Case(
            expression,
            Box::new(resolve_statement(state, *statement)?),
            label,
        )),
        Statement::Switch {
            expression,
            body,
            label,
            cases,
        } => Ok(Statement::Switch {
            expression,
            body: Box::new(resolve_statement(state, *body)?),
            label,
            cases,
        }),
    }
}
