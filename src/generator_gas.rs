use parser;

fn _generate_unop(op: parser::ParseUnOp) -> String {
    match op {
        parser::ParseUnOp::Negation => format!("  neg     %eax\n"),
        parser::ParseUnOp::BitwiseComplement => format!("  not     %eax\n"),
        parser::ParseUnOp::LogicalNegation => format!("  cmpl    $0, %eax\n  movl    $0, %eax\n  sete    %al\n"),
    }
}

fn _generate_factor(factor: parser::ParseFactor) -> String {
    match factor {
        parser::ParseFactor::Exp(e) => _generate_expression(*e),
        parser::ParseFactor::UnOp(op, x) => {
            let x_asm = _generate_factor(*x);
            let op_asm = _generate_unop(op);
            format!("{}{}", x_asm, op_asm)
        },
        parser::ParseFactor::Int(i) => format!("  movl    ${}, %eax\n", i),
    }
}

fn _generate_term(term: parser::ParseTerm) -> String {
    match term {
        parser::ParseTerm::Factor(f) => _generate_factor(*f),
        parser::ParseTerm::BinOp(lhs, op, rhs) => {
            match op {
                parser::ParseBinOp::Multiplication => {
                    let lhs_str = _generate_factor(*lhs);
                    let rhs_str = _generate_factor(*rhs);
                    format!("{}  pushl   %eax\n{}  popl    %ecx\n  imul    %ecx, %eax\n", lhs_str, rhs_str)
                },
                parser::ParseBinOp::Division => {
                    panic!("term division not implemented")
                },
                _ => panic!("expected multiplication or division, found {}", op)
            }
        }
    }
}

fn _generate_expression(expression: parser::ParseExp) -> String {
    match expression {
        parser::ParseExp::Term(t) => _generate_term(*t),
        parser::ParseExp::BinOp(lhs, op, rhs) => {
            match op {
                parser::ParseBinOp::Addition => {
                    let lhs_str = _generate_term(*lhs);
                    let rhs_str = _generate_term(*rhs);
                    format!("{}  pushl   %eax\n{}  popl    %ecx\n  addl    %ecx, %eax\n", lhs_str, rhs_str)
                },
                parser::ParseBinOp::Subtraction => {
                    panic!("exp subtraction not implemented")
                },
                _ => panic!("expected addition or subtraction, found {}", op)
            }
        },
    }
}

fn _generate_statement(statement: parser::ParseStatement) -> String {
    match statement {
        parser::ParseStatement::Return(exp) => {
            let exp_asm = _generate_expression(exp);
            format!("{}  ret\n", exp_asm)
        },
    }
}

fn _generate_function(function: parser::ParseFunction) -> String {
    match function {
        parser::ParseFunction::IntVoid(id, stmt) => {
            let stmt_asm = _generate_statement(stmt);
            format!("_{}\n_{}:\n{}", id, id, stmt_asm)
        },
    }
}

fn _generate_program(program: parser::ParseProgram) -> String {
    let f_asm = match program {
        parser::ParseProgram::Function(f) => _generate_function(f),
    };
    format!("  .align 4\n  .global {}", f_asm)
}

pub fn generate(program: parser::ParseProgram) -> String {
    _generate_program(program)
}
