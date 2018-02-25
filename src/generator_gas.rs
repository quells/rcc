use parser;

fn _generate_unop(op: parser::ParseUnOp) -> String {
    match op {
        parser::ParseUnOp::Negation => format!("  neg     %eax        /* Negate EAX */"),
        parser::ParseUnOp::BitwiseComplement => format!("  not     %eax        /* Bitwise Complement EAX */"),
        parser::ParseUnOp::LogicalNegation => format!(r"  cmpl    $0, %eax    /* ZF to (exp == 0) */
  movl    $0, %eax    /* zero out EAX without changing FLAGS */
  sete    %al         /* set AL register (low byte of EAX) to (ZF == true) */
                      /* EAX is logically negated */"),
    }
}

fn _generate_factor(factor: parser::ParseFactor) -> String {
    match factor {
        parser::ParseFactor::Exp(e) => _generate_expression(*e),
        parser::ParseFactor::UnOp(op, x) => {
            let x_asm = _generate_factor(*x);
            let op_asm = _generate_unop(op);
            format!(r"{}
{}", x_asm, op_asm)
        },
        parser::ParseFactor::Int(i) => format!("  movl    ${}, %eax    /* Set EAX to immediate value {} */", i, i),
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
                    format!(r"                      /* Set EAX to LHS of Multiplication */
{}
  pushl   %eax        /* Push EAX to the stack */
                      /* Set EAX to RHS of Multiplication */
{}
  popl    %ecx        /* Pop ECX from the stack */
  imul    %ecx, %eax  /* Multiply EAX and ECX, storing the result in EAX */", lhs_str, rhs_str)
                },
                parser::ParseBinOp::Division => {
                    let lhs_str = _generate_factor(*lhs);
                    let rhs_str = _generate_factor(*rhs);
                    format!(r"                      /* Set EAX to divisor */
{}
  pushl   %eax        /* Push EAX to the stack */
                      /* Set EAX to dividend */
{}
  movl    $0, %edx    /* Zero out EDX, which will hold the remainder of the division */
  cdq                 /* Expand EAX into EDX:EAX */
  popl    %ecx        /* Pop ECX from the stack */
  idivl   %ecx        /* Divide EDX:EAX by ECX and store the result in EAX */
                      /* The remainder is stored in EDX */", rhs_str, lhs_str)
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
                    format!(r"                      /* Set EAX to LHS of Addition */
{}
  pushl   %eax        /* Push EAX to the stack */
                      /* Set EAX to RHS of Addition */
{}
  popl    %ecx        /* Pop ECX from the stack */
  addl    %ecx, %eax  /* Add EAX and ECX, storing the result in EAX */", lhs_str, rhs_str)
                },
                parser::ParseBinOp::Subtraction => {
                    let lhs_str = _generate_term(*lhs);
                    let rhs_str = _generate_term(*rhs);
                    format!(r"                      /* Set EAX to RHS of Subtraction */
{}
  pushl   %eax        /* Push EAX to the stack */
                      /* Set EAX to LHS of Subtraction */
{}
  popl    %ecx        /* Pop ECX from the stack */
  subl    %ecx, %eax  /* Subtract ECX from EAX, storing the result in EAX */", rhs_str, lhs_str)
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
            format!(r"{}
  ret                 /* Return value of EAX */", exp_asm)
        },
    }
}

fn _generate_function(function: parser::ParseFunction) -> String {
    match function {
        parser::ParseFunction::IntVoid(id, stmt) => {
            let stmt_asm = _generate_statement(stmt);
            format!(r"_{}
_{}:
{}", id, id, stmt_asm)
        },
    }
}

fn _generate_program(program: parser::ParseProgram) -> String {
    let f_asm = match program {
        parser::ParseProgram::Function(f) => _generate_function(f),
    };
    format!(r"  .align 4
  .global {}", f_asm)
}

pub fn generate(program: parser::ParseProgram) -> String {
    _generate_program(program)
}
