#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Type {
    Int,
    FunType(usize),
}
