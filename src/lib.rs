extern crate failure_derive;

#[macro_use]
extern crate pest_derive;

mod constructs;
mod eval;
mod template_grammar;
mod util;
pub mod parser;
pub mod template;
