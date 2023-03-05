from ..x86_64 import *
from ...front.Lexer import lexer_from_src
from ...front.Parser import Parser


def parse_expr(src: str):
    lexer = lexer_from_src(src, '')
    tokens = list(lexer.lexfile())
    parser = Parser(src, tokens)
    parser.advance()
    expr = parser.parse_expr()


def test_parse():
    ast = parse_expr('some()\n')


def test_codegen():
    pass
