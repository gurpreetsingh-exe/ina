import pytest
from ..Lexer import TokenKind, lexer_from_src


# TODO: ('4839...439', [TokenKind.Int, TokenKind.DOT3, TokenKind.Int])
@pytest.mark.parametrize('input, expected', [
    ('fn main()//', [
        TokenKind.Fn,
        TokenKind.Ident,
        TokenKind.LPAREN,
        TokenKind.RPAREN,
    ]),
    ('fn', [TokenKind.Fn]),
    ('//', []),
    ('//fn main\nfn', [TokenKind.Fn]),
    ('3892.829', [TokenKind.Float]),
    ('...829', [TokenKind.DOT3, TokenKind.Int]),
    ('asd.//fgh', [TokenKind.Ident, TokenKind.DOT])
])
def test_lexer(input, expected):
    lexer = lexer_from_src(input, '')
    tokens = list(lexer.lexfile())
    for i, exp in enumerate(expected):
        assert tokens[i].kind == exp
    last_token = tokens[-1]
    loc = last_token.loc
    assert last_token.kind == TokenKind.EOF
    assert loc.offset + loc.len == len(input)
