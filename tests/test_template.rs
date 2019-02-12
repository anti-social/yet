use std::collections::HashMap;
use std::fs::read_dir;
use std::path::Path;

use failure::format_err;

use quire::ast::Ast;
use quire::emit_ast;

use yet::template::{parse_template, render, RenderContext};

fn env_vars_from_ast(env: Option<&Ast>)
    -> Result<HashMap<String, String>, failure::Error>
{
    let mut env_vars = HashMap::new();
    match env {
        None => {},
        Some(Ast::Map(.., map)) => {
            for (key, val) in map {
                match val {
                    Ast::Scalar(.., v) => env_vars.insert(key.clone(), v.clone()),
                    _ => return Err(format_err!("Env value must be a scalar")),
                };
            }
        },
        _ => return Err(format_err!("Env must be a map")),
    }
    Ok(env_vars)
}

fn test_file<P: AsRef<Path>>(test_file_path: P)
    -> Result<(), failure::Error>
{
    let tmpl_and_test_data = parse_template(test_file_path.as_ref())?;
    let (tmpl, test_data) = tmpl_and_test_data.split_first().unwrap();

    for test_case in test_data.iter() {
        let (env, values, result) = match test_case {
            Ast::Map(.., map) => {
                (
                    map.get("env"),
                    map.get("values"),
                    match map.get("result") {
                        Some(Ast::Map(.., res)) => res,
                        None => return Err(format_err!("`result` is mandatory")),
                        _ => return Err(format_err!("`result` must be a map")),
                    }
                )
            },
            _ => return Err(format_err!("Expected a map")),
        };

        let env_vars = env_vars_from_ast(env)?;
        let ctx = RenderContext::new(values, &env_vars);
        let render_result = render(tmpl, &ctx);

        if let Some(ok_res) = result.get("ok") {
            let rendered_ast = render_result?;
            let mut buf = Vec::<u8>::new();
            emit_ast(&rendered_ast, &mut buf)?;
            let output = String::from(std::str::from_utf8(&buf)?);
            buf.clear();
            emit_ast(&ok_res, &mut buf)?;
            let expected_output = String::from(std::str::from_utf8(&buf)?);

            assert_eq!(
                output,
                expected_output
            );
        } else if let Some(err_res) = result.get("err") {
            let expected_msg = match err_res {
                Ast::Map(.., err_map) => {
                    match err_map.get("msg") {
                        Some(Ast::Scalar(.., err_msg)) => err_msg,
                        None => return Err(format_err!("Missing `result.err.msg`")),
                        _ => return Err(format_err!("`result.err.msg` must be a scalar")),
                    }
                },
                _ => return Err(format_err!("`result.err` must be a map")),
            };
            match render_result {
                Ok(_) => return Err(format_err!("Expected error")),
                Err(e) => {
                    assert_eq!(
                        &format!("{}", e),
                        expected_msg
                    );
                }
            }
        } else {
            return Err(format_err!(
                "Result must contain `ok` or `err` key"
            ))
        }

    }

    Ok(())
}

#[test]
fn test_all_specs() -> Result<(), failure::Error> {
    for entry in read_dir("tests/specs")? {
        let test_file_path = entry?.path();
        if !test_file_path.is_file() { continue }

        test_file(test_file_path)?;
    }

//    assert!(false);

    Ok(())
}
