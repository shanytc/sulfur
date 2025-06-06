use std::path::Path;
use std::process::{Command, Stdio};
use sulfur_lang::backend::{Backend, IRTranslator as IR};
use sulfur_lang::frontend::{Lexer, Parser};

fn main() {
    // open file called main.sf
    let src = std::fs::read_to_string("src/main.sf")
        .expect("Failed to read source file");
    let lexer = Lexer::new(src.as_str());
    let mut parser = Parser::new(lexer);
    let ir = parser.parse_program();
    println!("--------------- IR OUTPUT ---------------");
    println!("{:?}", ir);
    println!("----------------------------------------");

    let (masm, libs) = IR::translate(&ir, &parser.functions, Backend::MASM32);

    let masm32 = "c:\\masm32\\bin\\ml.exe";
    let masm32linker = "c:\\masm32\\bin\\link.exe";
    let output_file = "c:\\masm32\\test.asm";
    let obj_path = Path::new(output_file).with_extension("obj");   // C:\...\test.obj
    let exe_path = Path::new(output_file).with_extension("exe");   // C:\...\test.obj
    let work_dir = Path::new(output_file).parent().unwrap();                     // directory to work in
    std::fs::write(output_file, &masm).expect("Unable to write file");


    // compile and run the MASM32 from c:\masm32\bin\ml.exe
    println!("----------------- MASM OUTPUT ------------------");
    println!("{}", masm);
    println!("-------------------------------------------------");

    if true {
        println!("\nMASM code generated and saved to: {}", output_file);
        // assemble, link the output file
        let status = std::process::Command::new(masm32)
            .arg("/c")
            .arg("/coff")
            .arg(format!("/Fo{}", obj_path.display()))   // <── put OBJ here
            .arg(output_file)
            .stdout(Stdio::null())        // hide normal output
            .stderr(Stdio::null())
            .status()
            .expect("Failed to execute assembler");

        if status.success() {
            println!("Assembly successful!");
            let libs = libs.iter()
                .map(|lib| format!("c:\\masm32\\lib\\{}", lib))
                .collect::<Vec<_>>();
            // link the object file to create an executable
            let link_status = std::process::Command::new(masm32linker)
                .arg(obj_path.as_os_str())
                .args(&libs)   // <── provides _printf
                .arg("/SUBSYSTEM:CONSOLE")
                .arg("/ENTRY:main")             // <── bypass the CRT startup
                .args(["/OPT:REF", "/OPT:ICF"]) // <── for debugging
                .arg(format!("/OUT:{}", exe_path.display())) // <── put EXE here
                .stdout(Stdio::null())        // hide normal output
                .stderr(Stdio::null())
                .status()
                .expect("Failed to execute linker");

            if link_status.success() {
                println!("Linking successful! Executable created: test.exe");

                println!("\n----------------- RUN OUTPUT ------------------");
                let run = Command::new(&exe_path)
                    .current_dir(work_dir)                 // optional; same dir as exe
                    .output()                              // captures stdout + stderr
                    .expect("failed to launch executable");
                print!("{}", String::from_utf8_lossy(&run.stdout));
                println!("\n---------------- {} ---------------------", run.status);
            } else {
                println!("Linking failed!");
            }
        } else {
            println!("Assembly failed!");
        }
    }
}

