use std::collections::HashMap;
use std::env;
use std::io::Write;
use std::path::PathBuf;

use argparse::{ArgumentParser, Store, List, StoreOption};

use quire::emit_ast;
use quire::ast::Ast;

use yet::template::{parse_template, parse_values, render};

fn main() -> Result<(), failure::Error> {
    let mut template_paths = Vec::<PathBuf>::new();
    let mut values_path = None::<PathBuf>;

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Yaml template rendering tool");
        ap.refer(&mut template_paths)
            .required()
            .add_argument("templates", List, "Templates to render");
        ap.refer(&mut values_path)
            .add_option(&["--values"], StoreOption, "Values file");

        ap.parse_args_or_exit();
    }

    let mut templates = Vec::<Ast>::new();
    for template_path in template_paths {
        templates.extend(parse_template(&template_path)?);
    }
    let values = match values_path {
        Some(p) => Some(parse_values(&p)?),
        None => None,
    };
    let env_vars = env::vars().collect::<HashMap<_, _>>();

    let mut out = std::io::stdout();
    for t in &templates {
        let rendered_asts = render(t, values.as_ref(), &env_vars)?;
        for rendered_ast in &rendered_asts {
            if rendered_asts.len() > 0 {
                writeln!(&mut out, "---")?;
            }
            emit_ast(&rendered_ast, &mut out)?;
        }
    }

    Ok(())
}
