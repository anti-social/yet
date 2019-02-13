use std::collections::BTreeMap;

use quire::ast::{Ast, NullKind, ScalarKind, Tag};

pub fn clone_tag(tag: &Tag) -> Tag {
    match tag {
        Tag::NonSpecific => Tag::NonSpecific,
        Tag::LocalTag(name) => Tag::LocalTag(name.clone()),
        Tag::GlobalTag(name) => Tag::GlobalTag(name.clone()),
    }
}

pub fn clone_scalar_kind(kind: &ScalarKind) -> ScalarKind {
    match kind {
        ScalarKind::Plain => ScalarKind::Plain,
        ScalarKind::Quoted => ScalarKind::Quoted,
    }
}

pub fn clone_null_kind(kind: &NullKind) -> NullKind {
    match kind {
        NullKind::Explicit => NullKind::Explicit,
        NullKind::Implicit => NullKind::Implicit,
    }
}

pub fn clone_ast(ast: &Ast) -> Ast {
    match ast {
        Ast::Map(pos, tag, map) => {
            let cloned_map = map.iter()
                .map(|(k, v)| (k.clone(), clone_ast(v)))
                .collect::<BTreeMap<_, _>>();
            Ast::Map(pos.clone(), clone_tag(tag), cloned_map)
        }
        Ast::Seq(pos, tag, seq) => {
            let cloned_seq = seq.iter()
                .map(|v| clone_ast(v))
                .collect::<Vec<_>>();
            Ast::Seq(pos.clone(), clone_tag(tag), cloned_seq)
        }
        Ast::Scalar(pos, tag, kind, val) => {
            Ast::Scalar(pos.clone(), clone_tag(tag), clone_scalar_kind(kind), val.clone())
        }
        Ast::Null(pos, tag, kind) => {
            Ast::Null(pos.clone(), clone_tag(tag), clone_null_kind(kind))
        }
    }
}
