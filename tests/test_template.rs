use std::collections::HashMap;
use std::path::Path;

use quire::emit_ast;

use yamplate::template::{parse_template, render, RenderContext};

#[test]
fn test_substitution() {
    let tmpl_and_test_data = parse_template(Path::new("tests/substitute_vars.yaml")).unwrap();
    let (tmpl, test_data) = tmpl_and_test_data.split_first().unwrap();
    let mut env_vars = HashMap::<String, String>::new();
    env_vars.insert("TEST_SUITE".to_string(), "unit".to_string());

    for values_and_output in test_data.iter().collect::<Vec<_>>().chunks(2) {
        let values = Some(*values_and_output.get(0).unwrap());
        let expected_ast = values_and_output.get(1).unwrap();
        let ctx = RenderContext::new(values, &env_vars);
        let rendered_ast = render(tmpl, &ctx).unwrap();

        let mut buf = Vec::<u8>::new();
        emit_ast(&rendered_ast, &mut buf).unwrap();
        let output = String::from(std::str::from_utf8(&buf).unwrap());
        buf.clear();
        emit_ast(&expected_ast, &mut buf).unwrap();
        let expected_output = String::from(std::str::from_utf8(&buf).unwrap());

        assert_eq!(
            output,
            expected_output
        );
    }

//    assert_eq!(1, 2);
}
