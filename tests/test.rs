use serde_json::json;

//use yaml_rust;

use yamplate;

#[test]
fn test_each() {
    let tmpl_and_test_data = yamplate::template::parse("tests/substitute_vars.yaml").unwrap();
    let (tmpl, test_data) = tmpl_and_test_data.split_first().unwrap();

    for values_and_output in test_data.iter().collect::<Vec<_>>().chunks(2) {
        let values = values_and_output.get(0).unwrap();
        let output = values_and_output.get(1).unwrap();
        let ctx = yamplate::template::RenderContext::new(values);
        let rendered_ast = yamplate::template::render(tmpl, &ctx).unwrap();
        dbg!(rendered_ast);
    }

//    let rendered_ast = yamplate::render(&ast, &values);
//    println!("rendered: {:?}", rendered_ast);

//    assert_eq!(1, 2);
}
