use std::process::Command;

fn main() {
    // Make the lua jit library
    Command::new("make")
        .current_dir("./luajit")
        .output()
        .expect("Failed to build LuaJIT");

    // Link the static lua jit library
    println!("cargo:rustc-link-search=all=./luajit/src");
    println!("cargo:rustc-link-lib=static=luajit");
}
