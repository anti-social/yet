use std::collections::HashMap;
use std::fs::read_dir;
use std::path::Path;

use anyhow::{bail, Error as AnyError};

use serde_yaml::Value;

use yet::template::{parse_template, render};

fn env_vars_from_ast(env: Option<&Value>)
    -> Result<HashMap<String, String>, AnyError>
{
    let mut env_vars = HashMap::new();
    match env {
        None => {},
        Some(Value::Mapping(map)) => {
            for (key, val) in map {
                match (key, val) {
                    (Value::String(k), Value::String(v)) => env_vars.insert(k.clone(), v.clone()),
                    _ => bail!("Env value must be a scalar"),
                };
            }
        },
        _ => bail!("Env must be a map"),
    }
    Ok(env_vars)
}

fn test_file<P: AsRef<Path>>(test_file_path: P)
    -> Result<(), AnyError>
{
    let tmpl_and_test_data = parse_template(test_file_path.as_ref())?;
    let (tmpl, test_data) = tmpl_and_test_data.split_first().unwrap();

    for test_case in test_data.iter() {
        let (env, values, result) = match test_case {
            Value::Mapping(map) => {
                (
                    map.get("env"),
                    map.get("values"),
                    match map.get("result") {
                        Some(Value::Mapping(res)) => res,
                        None => bail!("`result` is mandatory"),
                        _ => bail!("`result` must be a map"),
                    }
                )
            }
            _ => bail!("Expected a map"),
        };

        let env_vars = env_vars_from_ast(env)?;
        let render_result = render(tmpl, values, &env_vars);

        if let Some(ok_res) = result.get("ok") {
            let rendered_docs = render_result?;
            let mut output = String::new();
            match result.get("multi-doc") {
                Some(_) => {
                    output.push_str(&serde_yaml::to_string(&Value::Sequence(rendered_docs))?);
                },
                None => {
                    output.push_str(&serde_yaml::to_string(&rendered_docs[0])?);
                }
            }

            assert_eq!(
                output,
                serde_yaml::to_string(ok_res)?
            );
        } else if let Some(err_res) = result.get("err") {
            let expected_msg = match err_res {
                Value::Mapping(err_map) => {
                    match err_map.get("msg") {
                        Some(Value::String(err_msg)) => err_msg,
                        None => bail!("Missing `result.err.msg`"),
                        _ => bail!("`result.err.msg` must be a scalar"),
                    }
                },
                _ => bail!("`result.err` must be a map"),
            };
            match render_result {
                Ok(_) => bail!("Expected error"),
                Err(e) => {
                    assert_eq!(
                        &format!("{}", e),
                        expected_msg
                    );
                }
            }
        } else {
            bail!("Result must contain `ok` or `err` key")
        }
    }

    Ok(())
}

#[test]
fn test_all_specs() -> Result<(), AnyError> {
    for entry in read_dir("tests/specs")? {
        let test_file_path = entry?.path();
        if !test_file_path.is_file() { continue }

        println!(">>> {}", &test_file_path.to_string_lossy());
        test_file(test_file_path)?;
    }

//    assert!(false);

    Ok(())
}
