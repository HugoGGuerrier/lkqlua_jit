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

    // Statically link the Ada runtime
    println!("cargo:rustc-link-lib=static=gnat");
    println!("cargo:rustc-link-lib=static=gnarl");

    // Statically link libgmp (required by Liblkqllang)
    println!("cargo:rustc-link-lib=static=gmp");
}
