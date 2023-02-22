from ..x86_64 import *
# from ...Token import *
from ...front.Lexer import Lexer, lexer_from_src
from ...front.Parser import Parser


def codegen_stmt(ast, output):
    gen = Gen([], "")
    match ast:
        case list():
            for stmt in ast:
                gen.stmt(Stmt(stmt))
        case _:
            gen.stmt(Stmt(ast))
    assert gen.buf == output


def parse_expr(src: str):
    lexer = lexer_from_src(src, '')
    tokens = list(lexer.lexfile())
    parser = Parser(src, tokens)
    parser.advance()
    expr = parser.parse_expr()


def test_parse():
    ast = parse_expr('some()\n')


def test_codegen():
    codegen_stmt(Let(
        "_", PrimTy(PrimTyKind.I64),
        Expr(Literal(Lit.Int, "20"))
    ), "    mov qword ptr [rbp - 8], 20\n")
    codegen_stmt(Let(
        "_", PrimTy(PrimTyKind.F64),
        Expr(Literal(Lit.Float, "20.0"))
    ), "    mov rax, [rip + .L__unnamed_1]\n    mov qword ptr [rbp - 8], rax\n")
    codegen_stmt(Let(
        "_", PrimTy(PrimTyKind.I8),
        Expr(Literal(Lit.Int, "20"))
    ), "    mov qword ptr [rbp - 1], 20\n")
    codegen_stmt([
        Let(
            "_", PrimTy(PrimTyKind.F64),
            Expr(Literal(Lit.Float, "20.0"))
        ),
        Let(
            "a", RefTy(PrimTy(PrimTyKind.F64)),
            Expr(Unary(UnaryKind.AddrOf, Expr(Ident("_"))))
        ),
    ],
        "    mov rax, [rip + .L__unnamed_1]\n" +
        "    mov qword ptr [rbp - 8], rax\n" +
        "    lea rax, qword ptr [rbp - 8]\n" +
        "    mov qword ptr [rbp - 16], rax\n"
    )
