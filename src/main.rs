use std::path::Path;

use liblkqllang::AnalysisContext;
use lkqlua_jit::{intermediate_tree::Program, sources::SourceRepository};

fn main() {
    let mut source_repo = SourceRepository::new();
    let file_source = source_repo
        .get_file_source(&Path::new("test.lkql"))
        .unwrap();
    let ctx = AnalysisContext::create_default().unwrap();
    let unit = ctx
        .get_unit_from_file(&file_source.id(), None, false, None)
        .unwrap();

    let lowered = Program::lower_lkql_node(&unit.root().unwrap().unwrap(), &source_repo);
    println!("{}", unit.root().unwrap().unwrap().tree_dump(0).unwrap());
    println!();
    println!("{:?}", lowered);
    println!();
    println!("{}", lowered.unwrap());
}
