use std::collections::HashMap;
use std::env;
use std::io::Write;
use std::path::PathBuf;

use argparse::{ArgumentParser, Store, StoreOption};

use snafu::Whatever;
use snafu::prelude::*;

use yet::template::{parse_template, parse_values, render};

fn main() -> Result<(), Whatever> {
    let mut template_path = PathBuf::new();
    let mut values_path = None::<PathBuf>;

    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Yaml template rendering tool");
        ap.refer(&mut template_path)
            .required()
            .add_argument("template", Store, "Template to render");
        ap.refer(&mut values_path)
            .add_option(&["--values"], StoreOption, "Values file");

        ap.parse_args_or_exit();
    }

    let templates = parse_template(&template_path)
        .whatever_context("Could not parse template")?;
    let values = match values_path {
        Some(p) => Some(parse_values(&p).whatever_context("Could not parse values")?),
        None => None,
    };
    let env_vars = env::vars().collect::<HashMap<_, _>>();

    let mut out = std::io::stdout();
    for t in &templates {
        let rendered_asts = render(t, values.as_ref(), &env_vars)
            .whatever_context("Error rendering a template")?;
        for rendered_ast in &rendered_asts {
            if rendered_asts.len() > 0 {
                writeln!(&mut out, "---")
                    .whatever_context("Could not write to stdout")?;
            }
            serde_yaml::to_writer(&mut out, rendered_ast)
                .whatever_context("Could not write to stdout")?;
        }
    }

    Ok(())
}
