use yamplate;

#[test]
fn test_each() {
    let ast = yamplate::parse("tests/each.yaml");
    println!("original ast: {:?}", &ast);

    let values = yamplate::parse("tests/each_values.yaml");
    println!("values: {:?}", &values);

    let rendered_ast = yamplate::render(&ast, &values);
    println!("rendered: {:?}", rendered_ast);

    assert_eq!(1, 2);
}
