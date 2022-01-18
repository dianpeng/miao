
// This FType is only used in feedback related code
#[derive(Clone, PartialEq, Eq)]
pub enum FType {
    Int,
    Real,
    Boolean,
    Null,
    Str,
    List,
    Object,
    Function,
    NFunction,
    Iter,
    Unknown,
}
