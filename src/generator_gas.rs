use parser::*;

// fn _generate_unop(op: UnaryOp) -> String {
//     match op {
//         UnaryOp::Negation => format!("  neg     %eax        /* Negate EAX */"),
//         UnaryOp::BitwiseComplement => format!("  not     %eax        /* Bitwise Complement EAX */"),
//         UnaryOp::LogicalNegation => format!(r"  cmpl    $0, %eax    /* ZF to (exp == 0) */
//   movl    $0, %eax    /* zero out EAX without changing FLAGS */
//   sete    %al         /* set AL register (low byte of EAX) to (ZF == true) */
//                       /* EAX is logically negated */"),
//     }
// }

// fn _generate_factor(factor: Factor) -> String {
//     match factor {
//         Factor::Expr(e) => _generate_expression(*e),
//         Factor::UnOp(op, x) => {
//             let x_asm = _generate_factor(*x);
//             let op_asm = _generate_unop(op);
//             format!(r"{}
// {}", x_asm, op_asm)
//         },
//         Factor::Int(i) => format!("  movl    ${}, %eax    /* Set EAX to immediate value {} */", i, i),
//     }
// }

// fn _generate_term(term: Term) -> String {
//     match term {
//         Term::Factor(f) => _generate_factor(*f),
//         Term::BinOp(lhs, op, rhs) => {
//             match op {
//                 BinaryOp::Multiplication => {
//                     let lhs_str = _generate_factor(*lhs);
//                     let rhs_str = _generate_factor(*rhs);
//                     format!(r"                      /* Set EAX to LHS of Multiplication */
// {}
//   pushl   %eax        /* Push EAX to the stack */
//                       /* Set EAX to RHS of Multiplication */
// {}
//   popl    %ecx        /* Pop ECX from the stack */
//   imul    %ecx, %eax  /* Multiply EAX and ECX, storing the result in EAX */", lhs_str, rhs_str)
//                 },
//                 BinaryOp::Division => {
//                     let lhs_str = _generate_factor(*lhs);
//                     let rhs_str = _generate_factor(*rhs);
//                     format!(r"                      /* Set EAX to divisor */
// {}
//   pushl   %eax        /* Push EAX to the stack */
//                       /* Set EAX to dividend */
// {}
//   movl    $0, %edx    /* Zero out EDX, which will hold the remainder of the division */
//   cdq                 /* Expand EAX into EDX:EAX */
//   popl    %ecx        /* Pop ECX from the stack */
//   idivl   %ecx        /* Divide EDX:EAX by ECX and store the result in EAX */
//                       /* The remainder is stored in EDX */", rhs_str, lhs_str)
//                 },
//                 _ => panic!("expected multiplication or division, found {:?}", op)
//             }
//         }
//     }
// }

// fn _generate_expression(expression: Expr) -> String {
//     match expression {
//         Expr::Term(t) => _generate_term(*t),
//         Expr::BinOp(lhs, op, rhs) => {
//             match op {
//                 BinaryOp::Addition => {
//                     let lhs_str = _generate_term(*lhs);
//                     let rhs_str = _generate_term(*rhs);
//                     format!(r"                      /* Set EAX to LHS of Addition */
// {}
//   pushl   %eax        /* Push EAX to the stack */
//                       /* Set EAX to RHS of Addition */
// {}
//   popl    %ecx        /* Pop ECX from the stack */
//   addl    %ecx, %eax  /* Add EAX and ECX, storing the result in EAX */", lhs_str, rhs_str)
//                 },
//                 BinaryOp::Subtraction => {
//                     let lhs_str = _generate_term(*lhs);
//                     let rhs_str = _generate_term(*rhs);
//                     format!(r"                      /* Set EAX to RHS of Subtraction */
// {}
//   pushl   %eax        /* Push EAX to the stack */
//                       /* Set EAX to LHS of Subtraction */
// {}
//   popl    %ecx        /* Pop ECX from the stack */
//   subl    %ecx, %eax  /* Subtract ECX from EAX, storing the result in EAX */", rhs_str, lhs_str)
//                 },
//                 _ => panic!("expected addition or subtraction, found {:?}", op)
//             }
//         },
//     }
// }

// fn _generate_statement(statement: Statement) -> String {
//     match statement {
//         Statement::Return(exp) => {
//             let exp_asm = _generate_expression(exp);
//             format!(r"{}
//   ret                 /* Return value of EAX */", exp_asm)
//         },
//     }
// }

// fn _generate_function(function: Function) -> String {
//     match function {
//         Function::IntVoid(id, stmts) => {
//             let mut asm = String::new();
//             for stmt in stmts {
//                 let stmt_asm = _generate_statement(stmt);
//                 asm = format!(r"_{}
// _{}:
// {}", id, id, stmt_asm)
//             }
//             asm
//         },
//     }
// }

// fn _generate_program(program: Program) -> String {
//     let f_asm = match program {
//         Program::Function(f) => _generate_function(f),
//     };
//     format!(r"  .align 4
//   .global {}", f_asm)
// }

pub fn generate(program: Program) -> String {
    // _generate_program(program)
    "".to_string()
}
