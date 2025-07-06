//  tests/sulfur_masm32_tests.rs
//
//  `$ cargo test` will pick this up automatically.
//
//  The crate root (lib.rs) must expose `compile_to_masm()` exactly
//  as shown earlier.

mod masm32 {
    use std::path::{Path, PathBuf};
    use std::process::{Command, Stdio};
    use std::fs::write;
    use sulfur_lang::compile_to_masm;
    use std::sync::Mutex;

    static BUILD_LOCK: Mutex<()> = Mutex::new(());

    // Re-export the masm32 function for tests
    pub fn masm32(src: &str) -> (String, Vec<String>) {
        compile_to_masm(src)
    }

    fn execute_code(asm: String, libs: Vec<String>) -> std::io::Result<String> {
        let ml = r"C:\masm32\bin\ml.exe";
        let link = r"C:\masm32\bin\link.exe";
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
            .args(["/nologo", "/c", "/coff"])
            .arg(format!("/Fo{}", obj_path.display()))
            .arg(&asm_path)
            .stdout(Stdio::null())  // ← throw away “Assembling: …” (stdout)
            .status()?
            .success();
        assert!(ok, "ML.EXE failed");

        // link
        let full_libs: Vec<_> = libs.iter()
            .map(|l| format!(r"C:\masm32\lib\{l}"))
            .collect();
        let ok = Command::new(link)
            .arg("/nologo")
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

        let out = stdout.to_string().replace("\r\r\n", "\n");
        // remove last \n
        Ok(out.trim_end().to_string())
    }

    #[test]
    fn hello_world_generates_print() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
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

    #[cfg(windows)]
    #[test]
    fn const_fold_addition() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
        let src = r#"
        fn main() {
            let a = 2 + 3;
            let b = 5 - 2;
            let c = a + b;
            print("%d,%d,%d\n", a, b, c);
        }
    "#;
        let (asm, libs) = masm32(src);
        assert!(asm.contains("a_var dd 5"), "const-folding failed");
        assert!(asm.contains("b_var dd 3"), "const-folding failed");
        assert!(asm.contains("extrn printf:PROC"), "printf extrn missing");
        let output = execute_code(asm, libs).expect("Execution failed");
        assert_eq!(output, "5,3,8", "Output mismatch");
    }


    #[cfg(windows)]
    #[test]
    fn test_masm32_multiplication() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
        let src = r#"
            fn main() {
                let a = 2 * 3;
                let b = 4 * 5;
                let c = a * b;
                print("%d,%d,%d\n", a, b, c);
            }
        "#;
        let (asm, libs) = masm32(src);
        let output = execute_code(asm, libs).expect("Execution failed");
        assert_eq!(output, "6,20,120", "Multiplication output mismatch");
    }


    #[cfg(windows)]
    #[test]
    fn test_masm32_division() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
        let src = r#"
            fn main() {
                let a = 20 / 4;
                let b = 15 / 3;
                let c = a / b;
                print("%d,%d,%d\n", a, b, c);
            }
        "#;
        let (asm, libs) = masm32(src);
        let output = execute_code(asm, libs).expect("Execution failed");
        assert_eq!(output, "5,5,1", "Division output mismatch");
    }

    #[cfg(windows)]
    #[test]
    fn while_loop_emits_two_labels() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
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
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
        let src = r#"
        fn main() {
            let p = malloc(8);
            free(p);
        }
    "#;
        let (asm, libs) = masm32(src);

        // Each symbol should appear once in extrn list
        let extrn_malloc = asm.matches("extrn malloc:PROC").count();
        let extrn_free = asm.matches("extrn free:PROC").count();
        assert_eq!(extrn_malloc, 1, "malloc extrn duplicated");
        assert_eq!(extrn_free, 1, "free   extrn duplicated");

        // Their common msvcrt.lib should appear only once in include list
        let msvcrt_count = libs.iter().filter(|l| l.ends_with("msvcrt.lib")).count();
        assert_eq!(msvcrt_count, 1, "msvcrt.lib duplicated in libs vec");
    }

    #[cfg(windows)]
    #[test]
    fn hello_world_executes_ok() -> std::io::Result<()> {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
        let src = r#"fn main() { print("hi\n"); }"#;
        let (asm, libs) = masm32(src);
        assert_eq!(execute_code(asm, libs)?, "hi");
        Ok(())
    }

    #[cfg(windows)]
    #[test]
    fn test_masm32_pointers() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
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
        let output = execute_code(asm, vec!["msvcrt.lib".to_string()]).expect("Execution failed");
        let expected_output: String = (0..10).map(|i| format!("{}", i)).collect::<Vec<_>>().join("\n"); // add newline at the end
        // remove last newline for comparison
        assert_eq!(output, expected_output, "Pointer arithmetic or printing failed");
    }

    #[cfg(windows)]
    #[test]
    fn test_masm32_dereference() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
        let src = r#"
        fn main() {
            let a = 42;
            let p = &a; // Get the address of a
            print(*p); // Dereference p to get the value of a
        }
    "#;

        let (asm, _) = masm32(src);

        // check if printed value is correct
        let output = execute_code(asm, vec!["msvcrt.lib".to_string()]).expect("Execution failed");
        assert_eq!(output, "42", "Dereferencing failed");
    }

    #[cfg(windows)]
    #[test]
    fn test_masm32_placeholders() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
        let src = r#"
        fn main() {
            let a;
            let b = 10;
            a = &b; // a is a reference to b
            print("{},{}\n", *a, b);
        }
    "#;

        let (asm, libs) = masm32(src);

        // check if printf is used correctly
        assert!(asm.contains("extrn printf:PROC"), "printf extrn missing");

        // execute the code and check output
        let output = execute_code(asm, libs).expect("Execution failed");
        assert_eq!(output, "10,10", "Placeholder printing failed");
    }

    #[cfg(windows)]
    #[test]
    fn test_masm32_bitwise_and() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
        let src = r#"
            fn main() {
                let a = 5; // 0101 in binary
                let b = 3; // 0011 in binary
                let c = a & b; // Bitwise AND
                print("%d\n", c); // Should print 1 (0001 in binary)
            }
       "#;
        let (asm, libs) = masm32(src);

        // check if printf is used correctly
        assert!(asm.contains("extrn printf:PROC"), "printf extrn missing");

        // execute the code and check output
        let output = execute_code(asm, libs).expect("Execution failed");
        assert_eq!(output, "1", "Bitwise AND operation failed");
    }
    #[cfg(windows)]
    #[test]
    fn test_masm32_bitwise_or() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
        let src = r#"
            fn main() {
                let a = 5; // 0101 in binary
                let b = 3; // 0011 in binary
                let c = a | b; // Bitwise OR
                print("%d\n", c); // Should print 7 (0111 in binary)
            }
       "#;
        let (asm, libs) = masm32(src);

        // check if printf is used correctly
        assert!(asm.contains("extrn printf:PROC"), "printf extrn missing");

        // execute the code and check output
        let output = execute_code(asm, libs).expect("Execution failed");
        assert_eq!(output, "7", "Bitwise OR operation failed");
    }

    // pointer tests
    #[cfg(windows)]
    #[test]
    fn test_masm32_pointer_arithmetic() {
        let _lock = BUILD_LOCK.lock().unwrap(); // ensure single-threaded test execution
        let src = r#"
            fn main() {
                let a = malloc(4 * 10); // Allocate memory for 10 integers
                let i = 0;
                while (i < 10) {
                    *(a + i) = i * 2; // Store even numbers
                    i += 1;
                }
                i = 0;
                while (i < 10) {
                    print("%d\n", *(a + i)); // Print the values
                    i += 1;
                }
                free(a); // Free the allocated memory
            }
        "#;

        let (asm, libs) = masm32(src);
        let output = execute_code(asm, libs).expect("Execution failed");
        let expected = (0..10)
            .map(|i| (i * 2).to_string())
            .collect::<Vec<_>>()
            .join("\n");          // newline-separated, no trailing \n
        assert_eq!(output, expected, "Pointer arithmetic failed");
    }
}