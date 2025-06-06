use crate::shared::VAR_NAME_EXTENSION;
use std::collections::{HashMap, HashSet};
use crate::frontend::{IRNode, Literal, Type};
use crate::frontend::{Expr, PrintArg, Token};

static RUNTIME_LOOKUP: &[(&str, &str)] = &[
    // C runtime
    ("printf", "msvcrt.lib"),
    ("scanf",  "msvcrt.lib"),
    ("malloc", "msvcrt.lib"),
    ("free",   "msvcrt.lib"),
    ("memcpy", "msvcrt.lib"),
    // Win32 kernel (examples)
    ("CreateFileA", "kernel32.lib"),
    ("CloseHandle", "kernel32.lib"),
];

pub fn lit_label(idx: usize) -> String {
    format!("lit{}", idx)
}

pub fn lib_for(sym: &str) -> &'static str {
    RUNTIME_LOOKUP
        .iter()
        .find(|(s, _)| *s == sym)
        .map(|(_, lib)| *lib)
        .unwrap_or("msvcrt.lib")           // sensible default
}

fn masm_encode_string(src: &str) -> String {
    let mut parts: Vec<String> = Vec::new();
    let mut seg   = String::new();
    let mut chars = src.chars().peekable();

    // helper to flush current text segment (if not empty)
    let flush = |seg: &mut String, parts: &mut Vec<String>| {
        if !seg.is_empty() {
            parts.push(format!("\"{}\"", seg.replace('"', "\"\"")));
            seg.clear();
        }
    };

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.peek() {
                Some('n') => { chars.next(); flush(&mut seg, &mut parts); parts.push("13,10".into()); }
                Some('r') => { chars.next(); flush(&mut seg, &mut parts); parts.push("13".into());    }
                Some('t') => { chars.next(); flush(&mut seg, &mut parts); parts.push("9".into());     }
                Some('\\') => { chars.next(); seg.push('\\'); }      // keep backslash
                Some('"')  => { chars.next(); seg.push('"');  }      // keep quote
                _ => seg.push('\\'),                                 // unknown escape: keep '\'
            }
        } else {
            seg.push(ch);
        }
    }
    flush(&mut seg, &mut parts);

    parts.join(",")
}

pub fn gen_label(counter: &mut usize) -> String {
    let lbl = format!("label_{}", *counter);
    *counter += 1;
    lbl
}

pub fn gen_ptr_add(out: &mut String, op: &Token) {
    out.push_str("    imul eax, 4\n");           // index *= 4   (sizeof i32)
    match op {
        Token::Plus  => out.push_str("    add eax, ebx\n"),
        Token::Minus => {
            out.push_str("    sub ebx, eax\n");
            out.push_str("    mov eax, ebx\n");
        }
        _ => unreachable!(),
    }
}

pub fn emit_stmt(
    out: &mut String,
    stmt: &IRNode,
    var_decls: &Vec<(String, &Literal)>,
    lbl_counter: &mut usize,
    tail: &String,
    functions: &HashMap<String, (Vec<(String, Type)>, Type)>,
    lit_table: &mut HashMap<String, String>,
) {
    match stmt {
        IRNode::If {cond, then_branch, else_branch} => {
            let else_label = gen_label(lbl_counter);
            let end_label = gen_label(lbl_counter);

            // Evaluate condition
            masm_generator(out, cond, var_decls, functions, lit_table);
            out.push_str("    cmp eax, 0\n");
            out.push_str(&format!("    je {}\n", else_label));

            // Then branch
            for inner in then_branch {
                emit_stmt(out, inner, var_decls, lbl_counter, tail, functions, lit_table);
            }
            out.push_str(&format!("    jmp {}\n", end_label));

            // Else branch
            out.push_str(&format!("{}:\n", else_label));
            if let Some(else_branch) = else_branch {
                for inner in else_branch {
                    emit_stmt(out, inner, var_decls, lbl_counter, tail, functions, lit_table);
                }
            }
            out.push_str(&format!("{}:\n", end_label));
        }

        IRNode::While { cond, body } => {
            let start_label = gen_label(lbl_counter);
            let body_end_label = gen_label(lbl_counter); // End of loop body
            let exit_label = gen_label(lbl_counter);

            out.push_str(&format!("{}:\n", start_label));
            masm_generator(out, cond, var_decls, functions, lit_table);
            out.push_str("    cmp eax, 0\n");
            out.push_str(&format!("    je {}\n", exit_label));

            for inner in body {
                emit_stmt(out, inner, var_decls, lbl_counter, &body_end_label, functions, lit_table);
            }

            out.push_str(&format!("{}:\n", body_end_label));
            out.push_str(&format!("    jmp {}\n", start_label));
            out.push_str(&format!("{}:\n", exit_label));
        }

        IRNode::Assign { name, value } => {
            // let's add labels
            masm_generator(out, value, var_decls, functions, lit_table);
            out.push_str(&format!("    mov dword ptr [{}], eax\n", name));
        }

        // Handle Print statements
        IRNode::Print { args } => {
            let mut pushes = 0;
            // declare literals
            let mut fmt_builder = String::new();

            // push args in reverse order
            let original_fmt = args.first();
            let args: Vec<&PrintArg> = if let PrintArg::Literal(Literal::String(_)) = &args[0] {
                args.iter().skip(1).collect()
            } else {
                args.iter().collect()
            };

            for param in args.iter().rev() {
                match param {
                    PrintArg::Literal(Literal::Int(i)) => {
                        out.push_str(&format!("    push {}\n", i));
                        fmt_builder.push_str("%d");
                        pushes += 1;
                    },
                    PrintArg::Literal(Literal::Float(f)) => {
                        let lbl = intern_literal(lit_table, &format!("{}", f));
                        out.push_str(&format!("    fld  qword ptr [{}]\n", lbl)); // …optional
                        out.push_str(&format!("    push dword ptr [{}+4]\n", lbl));
                        out.push_str(&format!("    push dword ptr [{}]\n",   lbl));
                        fmt_builder.push_str("%f");
                        pushes += 2;
                    }
                    PrintArg::Literal(Literal::String(s)) => {
                        let lbl = intern_literal(lit_table, s);
                        out.push_str(&format!("    push offset {}\n", lbl));
                        fmt_builder.push_str("%s");
                        pushes += 1;
                    },
                    PrintArg::Variable(name) => match var_decls.iter().find(|(n, _)| n == name) {
                        Some((_, Literal::Int(_))) => {
                            out.push_str(&format!("    push dword ptr [{}]\n", name));
                            fmt_builder.push_str("%d");
                            pushes += 1;
                        },
                        Some((_, Literal::Float(_))) => {
                            out.push_str(&format!("    push dword ptr [{}+4]\n", name));
                            out.push_str(&format!("    push dword ptr [{}]\n", name));
                            fmt_builder.push_str("%f");
                            pushes += 2;
                        },
                        Some((_, Literal::String(_))) => {
                            out.push_str(&format!("    lea eax, {}\n", name));
                            out.push_str("    push eax\n");
                            fmt_builder.push_str("%s");
                            pushes += 1;
                        },
                        _  => {
                            panic!("Variable '{}' not found in declarations", name);
                        }
                    },
                    PrintArg::Expr(e) => {
                        masm_generator(out, e, var_decls, functions, lit_table);
                        out.push_str("    push eax\n");
                        fmt_builder.push_str("%d");
                        pushes += 1;
                    },
                }
            }

            let (_, enc_fmt) = match original_fmt {
                // user gave a literal → raw is *exactly* that string
                Some(PrintArg::Literal(Literal::String(s))) => {
                    let enc = masm_encode_string(s);  // encode once
                    (s.clone(), enc)
                }

                // no literal → we built an automatic "%d%f..." in fmt_text;
                // it still needs encoding for MASM.
                Some(_) => {
                    let raw = format!("{fmt_builder}\\n");
                    let enc = masm_encode_string(&raw);
                    (raw, enc)
                }

                None => panic!("print() called with no arguments"),
            };

            // Re-use existing literal label if present
            let fmt_lbl = intern_literal(lit_table, &enc_fmt);
            out.push_str(&format!("    push offset {fmt_lbl}\n"));
            pushes += 1;

            out.push_str("    call printf\n");
            out.push_str(&format!("    add esp, {}\n", pushes * 4));
        }
        IRNode::Store { dst, value} => {
            // rhs => edx
            masm_generator(out, value, var_decls, functions, lit_table);
            out.push_str("    mov edx, eax\n"); // store value in edx

            // address => eax
            if let Expr::Binary { left, op, right } = dst {
                if matches!(op, Token::Plus | Token::Minus) {
                    masm_generator(out, left, var_decls, functions, lit_table);
                    out.push_str("    push eax\n"); // save pointer base
                    masm_generator(out, right, var_decls, functions, lit_table);
                    out.push_str("    pop ebx\n"); // pop pointer base into EBX
                    gen_ptr_add(out, op); // calculate pointer address in EAX
                } else {
                    masm_generator(out, dst, var_decls, functions, lit_table);
                }
            } else {
                masm_generator(out, dst, var_decls, functions, lit_table);
            }

            out.push_str("    mov dword ptr [eax], edx\n"); // store value in address
        }
        IRNode::Call { name, args } => {
            // generate code for function call
            for arg in args {
                masm_generator(out, arg, var_decls, functions, lit_table);
                out.push_str("    push eax\n"); // push argument onto stack
            }
            out.push_str(&format!("    call {}\n", name));
            let num_args = args.len();
            out.push_str(&format!("    add esp, {}\n", num_args * 4)); // clean up stack
        }
        IRNode::Return(opt_expr) => {
            if let Some(expr) = opt_expr {
                masm_generator(out, expr, var_decls, functions, lit_table);
            } else {
                out.push_str("    xor eax, eax\n"); // return 0 if no expression
            }
            out.push_str(&format!("    jmp {}\n", tail)); // jump to the end of the function
        }
        _ => {}
    }
}


fn collect_libs(
    stmt: &IRNode,
    defined: &HashMap<String, (Vec<(String, Type)>, Type)>,
    libs: &mut Vec<String>,         // preserves order for the final MASM file
    extrns: &mut HashSet<String>,   // tracks *symbols* already declared
) {
    fn walk_expr(
        e: &Expr,
        defined: &HashMap<String, (Vec<(String, Type)>, Type)>,
        libs: &mut Vec<String>,
        extrns: &mut HashSet<String>,
    ) {
        match e {
            Expr::Call { name, args } => {
                if !defined.contains_key(name) {
                    // 1️⃣  make sure EXTRN appears once per symbol
                    extrns.insert(name.clone());

                    // 2️⃣  make sure the corresponding library line appears once
                    let lib = lib_for(name);
                    if !libs.contains(&lib.to_string()) {
                        libs.push(lib.to_string());
                    }
                }
                // recurse into arguments
                for a in args {
                    walk_expr(a, defined, libs, extrns);
                }
            }

            Expr::Unary  { expr, .. }        => walk_expr(expr, defined, libs, extrns),
            Expr::Binary { left, right, .. } => {
                walk_expr(left,  defined, libs, extrns);
                walk_expr(right, defined, libs, extrns);
            }
            Expr::Literal(_) | Expr::Variable(_) => {}
        }
    }

    match stmt {
        // nodes that hold expressions
        IRNode::Assign { value, .. } |
        IRNode::Return(Some(value))  => walk_expr(value, defined, libs, extrns),

        IRNode::Print { args }       => {
            extrns.insert("printf".into());

            let lib = lib_for("printf"); // usually "msvcrt.lib"
            if !libs.contains(&lib.to_string()) {
                libs.push(lib.to_string());
            }

            for p in args {
                if let PrintArg::Expr(e) = p {
                    walk_expr(e, defined, libs, extrns);
                }
            }
        }

        IRNode::Store { dst, value } => {
            walk_expr(dst,   defined, libs, extrns);
            walk_expr(value, defined, libs, extrns);
        }

        IRNode::Call { name, args }  => {
            let dummy = Expr::Call { name: name.clone(), args: args.clone() };
            walk_expr(&dummy, defined, libs, extrns);
        }

        // control-flow recursion
        IRNode::If { cond, then_branch, else_branch } => {
            walk_expr(cond, defined, libs, extrns);
            for n in then_branch { collect_libs(n, defined, libs, extrns); }
            if let Some(eb) = else_branch {
                for n in eb { collect_libs(n, defined, libs, extrns); }
            }
        }
        IRNode::While { cond, body } => {
            walk_expr(cond, defined, libs, extrns);
            for n in body { collect_libs(n, defined, libs, extrns); }
        }

        // everything else has no external calls
        IRNode::VarDecl(_)
        | IRNode::Return(None)
        | IRNode::Function { .. } => {}
    }
}


fn scan_expr(expr: &Expr, pool: &mut HashMap<String, String>) {
    match expr {
        Expr::Literal(Literal::String(s)) => {
            let enc = masm_encode_string(s);
            intern_literal(pool, &enc);
        }
        Expr::Binary { left, right, .. } => {
            scan_expr(left, pool);
            scan_expr(right, pool);
        }
        Expr::Call { args, .. } => {
            for arg in args {
                scan_expr(arg, pool);
            }
        }
        Expr::Literal(_) | Expr::Variable(_) => {
            // no literals to collect in these nodes
        }
        Expr::Unary { expr, .. } => {
            scan_expr(expr, pool);
        }
    }
}

fn intern_literal(pool: &mut HashMap<String, String>, s: &str) -> String {
    let idx = pool.len();
    pool.entry(s.to_owned())
        .or_insert_with(|| lit_label(idx))
        .clone()
}

fn collect_literals(stmt: &IRNode, pool: &mut HashMap<String, String>) {
    match stmt {
        IRNode::Print { args} => {
            if let Some(PrintArg::Literal(Literal::String(s))) = args.first() {
                let var = masm_encode_string(s.clone().as_str());
                intern_literal(pool, &var); // add to the pool → lit1, lit2, …
            } else {
                let mut fmt = String::new();
                for p in args {
                    match p {
                        PrintArg::Literal(Literal::Float(_))          => fmt.push_str("%f"),
                        PrintArg::Literal(Literal::String(_))         => fmt.push_str("%s"),
                        // Int literals, variables and expressions all use %d
                        _                                             => fmt.push_str("%d"),
                    }
                }
                fmt.push_str("\\n");          // trailing CR/LF just like emit_stmt
                let enc = masm_encode_string(&fmt);
                intern_literal(pool, &enc); // add to the pool → lit1, lit2, …
            }
        }
        IRNode::Assign {value, ..} | IRNode::Return(Some(value)) => scan_expr(value, pool),
        IRNode::If { cond, then_branch, else_branch } => {
            scan_expr(cond, pool);
            for inner in then_branch {
                collect_literals(inner, pool);
            }
            if let Some(else_branch) = else_branch {
                for inner in else_branch {
                    collect_literals(inner, pool);
                }
            }
        },
        IRNode::While { cond, body} => {
            scan_expr(cond, pool);
            for inner in body {
                collect_literals(inner, pool);
            }
        }
        IRNode::Call { args, ..} => {
            for arg in args {
                scan_expr(arg, pool);
            }
        },
        IRNode::VarDecl(_) | IRNode::Function {..} => {
            // no literals to collect in these nodes
        }
        _ => {}
    }
}

fn collect_vars<'a>(stmt: &'a IRNode, out: &mut Vec<(String, &'a Literal)>) {
    match stmt {
        IRNode::VarDecl(decls) => {
            for (name, value) in decls {
                // If the value is None, we assume it's 0
                let lit = value.as_ref().unwrap_or(&Literal::Int(0));
                out.push((name.clone(), lit));
            }
        }
        IRNode::If { cond: _, then_branch, else_branch } => {
            for s in then_branch { collect_vars(s, out); }
            if let Some(else_branch) = else_branch {
                for s in else_branch { collect_vars(s, out); }
            }
        }

        IRNode::While { body, ..} => {
            for s in body {
                collect_vars(s, out);
            }
        }
        _ => {}
    }
}

pub fn generate(
    nodes:      &[IRNode],
    functions:  &HashMap<String,(Vec<(String,Type)>,Type)>,
) -> (String, Vec<String>)
{
    let mut libraries = Vec::new();
    let mut externs = HashSet::new();
    let mut out = String::new();
    let mut lit_table: HashMap<String, String> = HashMap::new();

    // MASM boilerplate
    out.push_str(".386\n.model flat, c\n");
    out.push_str("option casemap:none\n\n");

    // declare all external functions
    nodes.iter().for_each(|n| {
        if let IRNode::Function { name: _, params: _, return_type: _, body} = n {
            for stmt in body {
                collect_libs(stmt, functions, &mut libraries, &mut externs);
            }
        }
    });

    for s in &externs {
        out.push_str(&format!("extrn {s}:PROC\n"));
    }

    if !externs.is_empty() {
        out.push('\n');
    }

    // all includelib lines in first-seen order
    for l in &libraries {
        out.push_str(&format!("includelib lib\\{l}\n"));
    }
    if !libraries.is_empty() {
        out.push('\n');
    }

    // collect variables
    let mut var_decls = Vec::new();
    for n in nodes {
        if let IRNode::Function { body, params, name: func_name, .. } = n {
            // parameters
            for (_name, _) in params {
                let v = format!("{func_name}_{_name}{VAR_NAME_EXTENSION}");
                var_decls.push((v, &Literal::Int(0)));
            }
            // every VarDecl anywhere in the body
            for stmt in body {
                collect_vars(stmt, &mut var_decls);
            }
        }
    }

    // data section
    out.push_str(".data\n");

    // declare variables
    for (name, value) in &var_decls {
        match value {
            Literal::Int(i) => out.push_str(&format!("{} dd {}\n", name, i)),
            Literal::Float(f) => out.push_str(&format!("{} real8 {}\n", name, f)),
            Literal::String(s) => {
                // escape \n as 13, 10
                out.push_str(&format!("{} db \"{}\",0\n", name, s))
            },
        }
    }

    // collect literals from all functions
    for n in nodes {
        if let IRNode::Function { name: _, params: _, return_type: _, body } = n {
            for stmt in body {
                collect_literals(stmt, &mut lit_table);
            }
        }
    }

    // declare literals
    for (src, lbl) in &lit_table {
        out.push_str(&format!("{} db {},0\n", lbl, src));
    }

    // code section
    out.push_str("\n.code\n");

    let mut lbl_counter = 0;

    for n in nodes {
        if let IRNode::Function { name: func_name, params, return_type, body } = n {
            // Generate function header (prologue)
            out.push_str(&format!("{} proc\n", func_name));
            out.push_str("    push ebp\n");
            out.push_str("    mov ebp, esp\n");

            // copy each parameter to local variable
            for (idx, (_name, _ptype)) in params.iter().enumerate() {
                let var_name = format!("{}_{}{VAR_NAME_EXTENSION}",func_name, _name.clone()); // append _var to the variable name
                out.push_str(&format!("    mov eax, dword ptr [ebp+{}]\n", 8 + idx * 4));
                out.push_str(&format!("    mov dword ptr [{}], eax\n", var_name));
            }

            // set up a "return label" for returns
            let func_tail = gen_label(&mut lbl_counter);

            for stmt in body.iter() {
                emit_stmt(&mut out, stmt, &var_decls, &mut lbl_counter, &func_tail, functions, &mut lit_table);
            }

            // remove 'jmp func_tail' if it is the last statement
            {
                let needle = format!("    jmp {}\n", func_tail);
                if out.ends_with(&needle) {
                    let new_len = out.len() - needle.len();
                    out.truncate(new_len); // remove the last jump to the tail
                }
            }

            out.push_str(&format!("{}:\n", func_tail));

            // Generate function epilogue
            // If the return type is void, we don't need to return anything
            if matches!(return_type, Type::Void) {
                out.push_str("    xor eax, eax\n");
            }
            out.push_str("    mov esp, ebp\n");
            out.push_str("    pop ebp\n");
            out.push_str("    ret\n");
            out.push_str(&format!("{} endp\n\n", func_name));
        }
    }
    out.push_str("end main\n");
    (out, libraries)
}

pub fn masm_generator(
        out: &mut String,
        expr: &Expr,
        var_decls: &Vec<(String, &Literal)>,
        functions: &HashMap<String, (Vec<(String, Type)>, Type)>,
        lit_table: &mut HashMap<String, String>,
    ){
        match expr {
            Expr::Literal(Literal::Int(i)) => {
                out.push_str(&format!("    mov eax, {}\n",i));
            }
            Expr::Literal(Literal::String(s)) => {
                let enc = masm_encode_string(s);
                let lbl = intern_literal(lit_table, &enc);
                out.push_str(&format!("    lea eax, {}\n", lbl));
            }
            Expr::Variable(name) => {
                let name = name.to_owned(); // append _var to the variable name
                if let Some((_, Literal::Int(_))) = var_decls.iter().find(|(n, _)| *n == name) {
                    out.push_str(&format!("    mov eax, dword ptr [{}]\n", name));
                } else if let Some((_, Literal::Float(_))) = var_decls.iter().find(|(n, _)| *n == name) {
                    out.push_str(&format!("    fld qword ptr [{}]\n", name)); // load float variable
                    out.push_str("    fistp dword ptr [eax]\n"); // convert to int and store in eax
                } else if let Some((_, Literal::String(_))) = var_decls.iter().find(|(n, _)| *n == name) {
                    out.push_str(&format!("    lea eax, {}\n", name)); // load address of string variable
                } else {
                    panic!("Variable '{}' not found in declarations", name);
                }
            }
            Expr::Binary { left, op , right } => {
                masm_generator(out, left, var_decls, functions, lit_table); // generate code for the left expression
                out.push_str("    push eax\n"); // push a left result onto stack
                masm_generator(out, right, var_decls, functions, lit_table); // generate code for right expression
                out.push_str("    pop ebx\n"); // pop left result into ebx

                match op {
                    Token::Plus => out.push_str("    add eax, ebx\n"), // add left and right results
                    Token::Minus => {
                        out.push_str("    sub ebx, eax\n");
                        out.push_str("    mov eax, ebx\n"); // subtract left from right
                    },
                    Token::Star => out.push_str("    imul eax, ebx\n"), // multiply left and right results
                    Token::Slash => {
                        // eax = left / right
                        out.push_str("    mov ecx, eax\n");         // save divisor (= right)
                        out.push_str("    mov eax, ebx\n");         // eax = dividend (= left)
                        out.push_str("    cdq\n");                  // sign-extend into edx
                        out.push_str("    idiv ecx\n");             // eax = quotient
                    },
                    Token::Percent => {
                        // eax = left % right
                        out.push_str("    mov ecx, eax\n");         // save divisor (= right)
                        out.push_str("    mov eax, ebx\n");         // eax = dividend (= left)
                        out.push_str("    cdq\n");                  // sign-extend into edx
                        out.push_str("    idiv ecx\n");             // edx = remainder
                        out.push_str("    mov eax, edx\n");         // result in eax
                    },
                    Token::Less
                    | Token::LessEqual
                    | Token:: Greater
                    | Token::GreaterEqual
                    | Token::EqualEqual
                    | Token::BangEqual => {
                        out.push_str("    cmp ebx, eax\n");
                        out.push_str("    mov eax, 0\n"); // default to false (0)
                        match op {
                            Token::Less => out.push_str("    setl al\n"), // set al to 1 if less
                            Token::LessEqual => out.push_str("    setle al\n"), // set al to 1 if less or equal
                            Token::Greater => out.push_str("    setg al\n"), // set al to 1 if greater
                            Token::GreaterEqual => out.push_str("    setge al\n"), // set al to 1 if greater or equal
                            Token::EqualEqual => out.push_str("    sete al\n"), // set al to 1 if equal
                            Token::BangEqual => out.push_str("    setne al\n"), // set al to 1 if not equal
                            _ => unreachable!("Unsupported comparison operator: {:?}", op)
                        }
                    },
                    Token::AndAnd => {
                        /* EBX = left, EAX = right */
                        out.push_str("    cmp ebx, 0\n");
                        out.push_str("    setne bl\n");       // bl = (left != 0)
                        out.push_str("    cmp eax, 0\n");
                        out.push_str("    setne al\n");       // al  = (right != 0)
                        out.push_str("    and al, bl\n");     // al = al & bl
                        out.push_str("    movzx eax, al\n");  // zero-extend to eax
                    },
                    Token::OrOr => {
                        /* EBX = left, EAX = right */
                        out.push_str("    cmp ebx, 0\n");
                        out.push_str("    setne bl\n");       // bl = (left != 0)
                        out.push_str("    cmp eax, 0\n");
                        out.push_str("    setne al\n");       // al  = (right != 0)
                        out.push_str("    or al, bl\n");      // al = al | bl
                        out.push_str("    movzx eax, al\n");  // zero-extend to eax
                    }
                    _ => unreachable!("Unsupported operator generation: {:?}", op)
                }
            }
            Expr::Unary { op: Token::Star, expr } => {
                // *(ptr +- idx)
                if let Expr::Binary { left, op, right } = &**expr {
                    if matches!(op, Token::Plus | Token::Minus) {
                        masm_generator(out, left, var_decls, functions, lit_table); // evaluate pointer base
                        out.push_str("    push eax\n");
                        masm_generator(out, right, var_decls, functions, lit_table); // evaluate index
                        out.push_str("    pop ebx\n"); // pop pointer base into EBX
                        gen_ptr_add(out, op); // calculate pointer address in EAX
                        out.push_str("    mov eax, dword ptr [eax]\n"); // dereference pointer
                        return;
                    }
                }

                // fallback
                masm_generator(out, expr, var_decls, functions, lit_table);
                out.push_str("    mov eax, dword ptr [eax]\n"); // dereference pointer
            }
            Expr::Call { name, args } => {
                // Evaluate args right-to-left, push them on the stack
                // We assume each argument fits in EAX after evaluating its expression.
                for arg in args.iter().rev() {
                    masm_generator(out, arg, var_decls, functions, lit_table);
                    out.push_str("    push eax\n");
                }
                // Now call the function
                out.push_str(&format!("    call {}\n", name));
                // After call, the return value (if any) is in EAX.
                // The caller is responsible for cleaning up: add esp, <args.len()*4>
                let cleanup_bytes = args.len() * 4;
                if cleanup_bytes > 0 {
                    out.push_str(&format!("    add esp, {}\n", cleanup_bytes));
                }
                // At this point, EAX holds the returned integer (or 0 if void).
            }
            _ => panic!("Unsupported expression type for MASM generation: {:?}", expr)
        }
    }