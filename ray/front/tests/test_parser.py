import pytest
from ...Ast import *
from ...front.Parser import *
from ...front.Lexer import *
from ...front.fmt import *


def parse_expr(src: str) -> Expr:
    lexer = lexer_from_src(src, '')
    tokens = list(lexer.lexfile())
    parser = Parser(src, tokens)
    return parser.parse_expr()


def parse_stmt(src: str) -> Stmt:
    lexer = lexer_from_src(src, '')
    tokens = list(lexer.lexfile())
    parser = Parser(src, tokens)
    return parser.parse_stmt()


def parse(src: str) -> Module:
    lexer = lexer_from_src(src, '')
    tokens = list(lexer.lexfile())
    parser = Parser(src, tokens)
    # advance to set initial state
    return parser.parse()


ast_renderer = AstRenderer()


@pytest.mark.parametrize('input', [
    ('20 + 50'),
    ('20.20 + 50.5383'),
    ("a * b + c"),
    ("a == b == c == d"),
    ('-20.4 + *ptr * **ptr == !b != &c < 50.494'),
    ('"string" + 349 + 0.493 + true'),
    ('func(20.49)'),
    ('!!*****a'),
    ("""\
if ***a + b < some(t) {
    true
}"""),
])
def test_parse_expr(input):
    assert ast_renderer.expr(parse_expr(input)) == input


@pytest.mark.parametrize('input', [
    'let a: i32 = 20;',
    'let a = 20;',
    'let a = if a {};',

    # nested if else block without semi-colon
    """\
let a = if a {
    if a {
        true
    } else {}
} else {
    false
};""",

    # nested if else block with semi-colon
    """\
let a = if a {
    if a {
        true
    } else {};
} else {
    false
};""",

    """\
let a = if a {
    20;
};""",

    """\
let a = if a {
    20;
    50
};""",

    """\
let a = loop {
    expr
};""",

    """\
let a = func(loop {
    if a {
        expr
    }
    expr
});""",
])
def test_parse_stmt(input):
    assert ast_renderer.stmt(parse_stmt(input)) == input


@pytest.mark.parametrize('input', [
    """\
fn main() {
    let a = 4;
    let b = 5;
    let d = 4;
    let x = 1000;
    if a < b {
        let c = b;
        d = 2;
    }
    let c = 4;
    b * d + c;
    if b < b {
        let e = c;
        e
    }
    a
}""",
])
def test_parse(input):
    assert ast_renderer.render(parse(input)) == input
