use pest::Parser;

#[derive(Parser)]
#[grammar = "template_grammar.pest"]
pub(crate) struct TemplateParser;
