pub mod cst;
pub mod tokenizer;

use serde::{Deserialize, Serialize};

pub use cst::*;
pub use tokenizer::*;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SourceLocation {
    pub line: usize,
    pub col: usize,
    pub len: usize,
    pub raw_offset: usize,
}
