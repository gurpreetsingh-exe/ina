import pytest
from ...Ast import Module
from ...utils import File
from ...sema.tychk import TyCheck, TyEnv
from ..IrGen import IRGen
from ...front.Lexer import lexer_from_src
from ...front.Parser import Parser


def parse_stmt(src: str):
    lexer = lexer_from_src(src, '')
    tokens = list(lexer.lexfile())
    parser = Parser(src, tokens)
    parser.advance()
    return parser.parse_stmt()


def parse_stmt_lowered(src: str):
    stmt = parse_stmt(src)
    dummy = Module([])
    typeck = TyCheck(dummy, File("", src))
    typeck.env = TyEnv()
    typeck.visit_stmt(stmt)
    irgen = IRGen(dummy)
    irgen.lower_stmt(stmt)
    return "\n".join(map(str, irgen.instructions))


@pytest.mark.parametrize('input, expected', [
    ('let a = 20;', '    %0 = alloc i64\n    store %0, 20'),
    ('let a: i8 = 20;', '    %0 = alloc i8\n    store %0, 20'),
    ('let a = "nice";', '    %0 = alloc str\n    store %0, .L__unnamed_1'),
    ('let a = [0; 10];',
     '    %0 = alloc [i64; 10]\n    %1 = call memset (%0, 0, 10)'),
])
def test_codegen(input, expected):
    assert parse_stmt_lowered(input) == expected
