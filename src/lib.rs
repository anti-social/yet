extern crate quire;
use quire::ast::Ast;

use std::fs::File;
use std::rc::Rc;
use std::io::Read;
use quire::Pos;
use quire::ast::Tag;
use std::collections::BTreeMap;
use quire::ast::NullKind;
use quire::ast::ScalarKind;

pub fn parse(fname: &str) -> Ast {
    let ref mut content = String::new();
    File::open(fname).unwrap().read_to_string(content);

    let errors = quire::ErrorCollector::new();
    quire::raw_parse(Rc::new(fname.to_string()), content, |doc| {
        quire::ast::process(&quire::Options::default(), doc, &errors)
    }).unwrap()
}

fn clone_tag(tag: &Tag) -> Tag {
    match tag {
        Tag::NonSpecific => Tag::NonSpecific,
        Tag::LocalTag(name) => Tag::LocalTag(name.clone()),
        Tag::GlobalTag(name) => Tag::GlobalTag(name.clone()),
    }
}

fn clone_scalar_kind(kind: &ScalarKind) -> ScalarKind {
    match kind {
        ScalarKind::Plain => ScalarKind::Plain,
        ScalarKind::Quoted => ScalarKind::Quoted,
    }
}

fn path<'a>(ast: &'a Ast, path: &str) -> &'a Ast {
    let mut cur_node = ast;
    path.split('.')
        .for_each(|p| {
            println!("part: {}", p);
            let map = match cur_node {
                Ast::Map(pos, tag, map) => map,
                _ => panic!("Expected a mapping"),
            };
            let v = match map.get(p) {
                Some(v) => v,
                None => panic!("Missing key: {}", p),
            };
            cur_node = v;
        });
    cur_node
}

pub fn render(ast: &Ast, values: &Ast) -> Ast {
    let position = Pos {
        filename: Rc::new("".to_string()),
        indent: 0,
        line: 0,
        line_start: true,
        line_offset: 0,
        offset: 0,
    };

    match ast {
        Ast::Map(pos, tag, map) => {
            if let Tag::LocalTag(tag_name) = tag {
                if tag_name == "*Each" {
                    println!("Each found!");
                    let values_node = match map.get("values").unwrap() {
                        Ast::Scalar(_, _, _, v) => {
                            let mut splitted = v.splitn(2, '.');
                            let prefix = splitted.next().unwrap();
                            path(values, splitted.next().unwrap())
                        }
                        _ => panic!("values of each must be string")
                    };
                    match values_node {
                        Ast::Seq(_, _, seq) => {

                        }
                        _ => panic!("values must be sequence")
                    }

                    println!("each values: {:?}", values_node);
                    let each_loop = map.get("loop").unwrap();
                    let rendered_map = BTreeMap::<String, Ast>::new();

                }
            }
            let rendered_map = map.iter()
                .map(|(k, v)| {
                    (k.clone(), render(v, values))
                })
                .collect::<BTreeMap<_,_>>();
            Ast::Map(pos.clone(), clone_tag(tag), rendered_map)
        }
        Ast::Seq(pos, tag, seq) => {
            let rendered_seq = seq.iter()
                .map(|a| {
                    render(a, values)
                })
                .collect::<Vec<_>>();
            Ast::Seq(pos.clone(), clone_tag(tag), rendered_seq)
        }
        Ast::Scalar(pos, tag, kind, val) => {
            Ast::Scalar(pos.clone(), clone_tag(tag), clone_scalar_kind(kind), val.clone())
        }
        Ast::Null(pos, tag, kind) => {
            Ast::Null(position.clone(), Tag::NonSpecific, NullKind::Explicit)
        }
    }
}
