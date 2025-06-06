//  tests/sulfur_masm32_tests.rs
//
//  `$ cargo test` will pick this up automatically.
//
//  The crate root (lib.rs) must expose `compile_to_masm()` exactly
//  as shown earlier.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::fs::write;
use sulfur_lang::compile_to_masm;

fn masm32(src: &str) -> (String, Vec<String>) {
    compile_to_masm(src)
}

/* ================================================================
   1. BASIC “NO-PANIC + IMPORTS” CHECK
   ================================================================ */
#[test]
fn hello_world_generates_printf() {
    let src = r#"
        fn main() {
            print("hello world\n");
        }
    "#;

    let (asm, _libs) = masm32(src);

    // compilation completed if we reached here
    assert!(asm.contains("extrn printf:PROC"), "printf import missing!");
    assert!(
        asm.contains("hello world") || asm.contains("13,10"),
        "string literal not encoded in .data section",
    );
}

#[test]
fn const_fold_addition() {
    let src = r#"
        fn main() {
            let a = 2 + 3;
            print("%d\n", a);
        }
    "#;
    let (asm, _) = masm32(src);
    assert!(asm.contains("a_var dd 5"), "const-folding failed");
}

#[test]
fn while_loop_emits_two_labels() {
    let src = r#"
        fn main() {
            let i = 0;
            while (i < 3) { i += 1; }
        }
    "#;
    let (asm, _) = masm32(src);

    // crude heuristic: the emitter always makes a start+exit label
    let label_count = asm.matches("label_").count();
    assert!(label_count >= 2, "loop codegen didn’t create labels");
}

#[test]
fn extern_symbols_and_libs_are_unique() {
    let src = r#"
        fn main() {
            let p = malloc(8);
            free(p);
        }
    "#;
    let (asm, libs) = masm32(src);

    // Each symbol should appear once in extrn list
    let extrn_malloc = asm.matches("extrn malloc:PROC").count();
    let extrn_free   = asm.matches("extrn free:PROC").count();
    assert_eq!(extrn_malloc, 1, "malloc extrn duplicated");
    assert_eq!(extrn_free,   1, "free   extrn duplicated");

    // Their common msvcrt.lib should appear only once in include list
    let msvcrt_count = libs.iter().filter(|l| l.ends_with("msvcrt.lib")).count();
    assert_eq!(msvcrt_count, 1, "msvcrt.lib duplicated in libs vec");
}

fn execute_code(asm: String, libs: Vec<String>) -> std::io::Result<String> {
    let ml    = r"C:\masm32\bin\ml.exe";
    let link  = r"C:\masm32\bin\link.exe";
    if !Path::new(ml).exists() || !Path::new(link).exists() {
        eprintln!("ML/LINK not found – skipping execution test");
        return Ok(String::new());
    }

    // scratch dir under target/ so `cargo clean` wipes it
    let scratch: PathBuf = {
        let p = Path::new(env!("CARGO_TARGET_TMPDIR")).join("sulfur_exec");
        std::fs::create_dir_all(&p)?;
        p
    };

    let asm_path = scratch.join("test.asm");
    let obj_path = scratch.join("test.obj");
    let exe_path = scratch.join("test.exe");
    write(&asm_path, asm)?;

    // assemble
    let ok = Command::new(ml)
        .args(["/c", "/coff"])
        .arg(format!("/Fo{}", obj_path.display()))
        .arg(&asm_path)
        .status()?
        .success();
    assert!(ok, "ML.EXE failed");

    // link
    let full_libs: Vec<_> = libs.iter()
        .map(|l| format!(r"C:\masm32\lib\{l}"))
        .collect();
    let ok = Command::new(link)
        .arg(&obj_path)
        .args(&full_libs)
        .args(["/SUBSYSTEM:CONSOLE", "/ENTRY:main", "/NODEFAULTLIB"])
        .arg(format!("/OUT:{}", exe_path.display()))
        .status()?
        .success();
    assert!(ok, "LINK.EXE failed");

    // run exe, capture stdout
    let out = Command::new(&exe_path)
        .current_dir(&scratch)
        .output()?;
    let stdout = String::from_utf8_lossy(&out.stdout);

    if !out.status.success() {
        eprintln!("Executable failed with status: {}", out.status);
    }

    Ok(stdout.trim().to_string())
}

#[cfg(windows)]
#[test]
fn hello_world_executes_ok() -> std::io::Result<()> {
    // ── skip fast if the tools aren’t present ─────────────────────

    let src = r#"fn main() { print("hi\n"); }"#;
    let (asm, libs) = masm32(src);
    assert_eq!(execute_code(asm, libs)?, "hi");

    Ok(())
}


#[cfg(windows)]
#[test]
fn test_masm32_pointers() {
    let src = r#"
        fn main() {
            let a, idx = 0;
            a = malloc(10 * 4); // Allocate memory for 10 integers
            while (idx < 10) {
                *(a + idx) = idx;
                idx += 1; // Increment the index
            }

            idx = 0;
            while (idx < 10) {
                print(*(a + idx)); // Print the value at the current index
                idx += 1; // Increment the index
            }

            free(a); // Free the allocated memory
        }
    "#;

    let (asm, _) = masm32(src);

    // check if printed values are correct
    let output = execute_code(asm, vec!["msvcrt.lib".to_string()]).expect("Execution failed").replace("\r\r\n", "\n");
    let expected_output: String = (0..10).map(|i| format!("{}", i)).collect::<Vec<_>>().join("\n");
    // remove last newline for comparison
    assert_eq!(output, expected_output, "Pointer arithmetic or printing failed");

}