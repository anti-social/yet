use serde_json::json;

use yaml_rust;

use yamplate;

#[test]
fn test_each() {
    let ast = yaml_rust::YamlLoader::load_from_str(
        "---
- !Test 1
-
  test:
    a: 1
---
b: !!bool 'true'
"
    );
    dbg!(ast);

//    let ast = yamplate::template::parse("tests/substitute_vars.yaml");
//    dbg!(ast);

//    let values = yamplate::parse("tests/each_values.yaml");
//    println!("values: {:?}", &values);
    let values = json!({
        "values": {
            "str": "test variable",
            "bool": true,
        }
    });

//    let rendered_ast = yamplate::render(&ast, &values);
//    println!("rendered: {:?}", rendered_ast);

    assert_eq!(1, 2);
}
