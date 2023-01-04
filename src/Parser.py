from __future__ import annotations
from typing import Callable, Generator, List

from Ast import *
from Token import *
from utils import panic

class Parser:
    def __init__(self, src: str, tokens: List[Token]):
        self.tokens = tokens
        self.src = src
        self.id = -1
        self.prev: Token | None = None
        self.t: Token | None = None

    def check(self) -> bool:
        return self.id < len(self.tokens) - 1

    def advance(self):
        if self.check():
            if self.id >= 0:
                self.prev = self.tokens[self.id]
            self.id += 1
            self.t = self.tokens[self.id]

    def eat_if_present(self, kind: TokenKind) -> bool:
        if not self.t:
            return False
        present = self.t.kind == kind
        if present:
            self.advance()
        return present

    def peek(self) -> Token:
        if self.check():
            return self.tokens[self.id + 1]
        else:
            panic("End of stream")

    def expect(self, kind: TokenKind) -> Token:
        if not self.t:
            panic(f"unexpected eof")

        if self.t.kind == kind:
            curr = self.t
            self.advance()
            return curr
        else:
            panic(f"expected `{kind}` got `{self.t.kind}` ({self.t.raw(self.src)})")

    def parse_ty(self) -> Ty:
        assert self.t != None
        match self.t.kind:
            case TokenKind.Ident:
                ty = self.expect(TokenKind.Ident).raw(self.src)
                if prim_ty := get_prim_ty(ty):
                    return Ty(prim_ty)
                else:
                    return Ty(ty)
            case _:
                panic("unexpected type")

    def parse_arg(self) -> FnArg:
        assert self.t != None
        if self.t.kind == TokenKind.DOT3:
            self.advance()
            return Variadic()
        ident = self.expect(TokenKind.Ident)
        ident = ident.raw(self.src)
        if self.t.kind != TokenKind.COLON:
            panic("expected `:`")
        else:
            self.advance()
            ty = self.parse_ty()
            return Arg(ident, ty)

    def parse_fn_args(self) -> Generator:
        assert self.t != None
        self.expect(TokenKind.LPAREN)

        while self.check() and self.t.kind != TokenKind.RPAREN:
            yield self.parse_arg()
            if self.t.kind == TokenKind.COMMA:
                self.advance()

        self.expect(TokenKind.RPAREN)

    def parse_ret_ty(self) -> Ty | None:
        assert self.t != None
        if self.t.kind == TokenKind.ARROW:
            self.advance()
            return self.parse_ty()

    def compound(self, tok_list, callback: Callable[[], Expr]) -> Expr:
        assert self.t != None
        left = callback()
        while self.check() and self.t.kind in tok_list:
            op = self.t
            self.advance()
            right = callback()
            left = Expr(Binary(op, left, right))

        return left

    def token_to_lit(self) -> Lit:
        assert self.t != None
        match self.t.kind:
            case TokenKind.Int:
                return Lit.Int
            case TokenKind.Float:
                return Lit.Float
            case TokenKind.Str:
                return Lit.Str
            case TokenKind.Bool:
                return Lit.Bool
            case _:
                panic("unexpected literal")

    def parse_call(self) -> Call:
        assert self.t != None
        name = self.expect(TokenKind.Ident).raw(self.src)
        self.expect(TokenKind.LPAREN)
        args = []
        while self.check() and self.t.kind != TokenKind.RPAREN:
            args.append(self.parse_expr())
            if self.t.kind == TokenKind.COMMA:
                self.advance()
        self.expect(TokenKind.RPAREN)
        return Call(name, args)

    def parse_primary(self) -> Expr:
        assert self.t != None
        match self.t.kind:
            case TokenKind.Ident:
                next_ = self.peek()
                if next_.kind == TokenKind.LPAREN:
                    return Expr(self.parse_call())
                else:
                    return Expr(Ident(self.expect(TokenKind.Ident).raw(self.src)))
            case TokenKind.Int | TokenKind.Float | TokenKind.Bool | TokenKind.Str:
                value = self.t.raw(self.src)
                lit_kind = self.token_to_lit()
                self.advance()
                return Expr(Literal(lit_kind, value))
            case TokenKind.If:
                self.advance()
                cond = self.parse_expr()
                body = self.parse_block()
                elze = None
                if self.t.kind == TokenKind.Else:
                    self.advance()
                    elze = self.parse_block()
                return Expr(If(cond, body, elze))
            case _:
                panic(f"unreachable {self.t.kind}")

    def parse_unary(self) -> Expr:
        assert self.t != None
        if self.t.kind in [TokenKind.MINUS, TokenKind.BANG, TokenKind.AMPERSAND]:
            op = self.t
            self.advance()
            right = self.parse_unary()
            return Expr(Unary(op, right))

        return self.parse_primary()

    def parse_factor(self) -> Expr:
        assert self.t != None
        prim = self.compound([TokenKind.STAR, TokenKind.SLASH], self.parse_unary)
        if self.t.kind == TokenKind.As:
            self.advance()
            ty = self.parse_ty()
            prim = Expr(Cast(prim, ty))
        return prim

    def parse_term(self) -> Expr:
        assert self.t != None
        return self.compound([TokenKind.PLUS, TokenKind.MINUS], self.parse_factor)

    def parse_comparison(self) -> Expr:
        assert self.t != None
        return self.compound([TokenKind.LT, TokenKind.GT], self.parse_term)

    def parse_assign(self) -> Expr:
        assert self.t != None
        left = self.parse_comparison()
        while self.t.kind == TokenKind.EQ:
            self.advance()
            init = self.parse_assign()
            if isinstance(left, Ident):
                return Expr(Assign(left.name, init))
            else:
                panic(f"Assignment expression expected `Ident` but got `{left}`")
        return left

    def parse_expr(self) -> Expr:
        assert self.t != None
        return self.compound([TokenKind.BANGEQ, TokenKind.EQ2], self.parse_assign)

    def parse_let(self) -> Let:
        assert self.t != None
        self.expect(TokenKind.Let)
        name = self.expect(TokenKind.Ident).raw(self.src)
        ty = None
        if self.t.kind == TokenKind.COLON:
            self.advance()
            ty = self.parse_ty()
        self.expect(TokenKind.EQ)
        init = self.parse_expr()
        return Let(name, ty, init)

    def parse_stmt(self) -> Stmt:
        assert self.t != None
        stmt = None
        match self.t.kind:
            case TokenKind.Let:
                stmt = self.parse_let()
                self.expect(TokenKind.SEMI)
            case TokenKind.LCURLY:
                stmt = self.parse_block()
                self.eat_if_present(TokenKind.SEMI)
            case _:
                stmt = self.parse_expr()
                self.eat_if_present(TokenKind.SEMI)
        return Stmt(stmt)

    def parse_block(self) -> Block:
        assert self.t != None
        self.expect(TokenKind.LCURLY)
        stmts = []
        while self.check() and self.t.kind != TokenKind.RCURLY:
            stmts.append(self.parse_stmt())
        self.expect(TokenKind.RCURLY)
        return Block(stmts)

    def parse_fn(self, is_extern=False, abi=None) -> Fn:
        assert self.t != None
        self.advance()
        ident = self.expect(TokenKind.Ident).raw(self.src)
        args = list(self.parse_fn_args())
        ret_ty = self.parse_ret_ty()
        body = None
        if self.t.kind == TokenKind.LCURLY:
            body = self.parse_block()
        return Fn(ident, args, ret_ty, body, is_extern, abi)

    def parse(self):
        self.advance()
        assert self.t != None
        while self.check():
            match self.t.kind:
                case TokenKind.Extern:
                    self.advance()
                    abi = None
                    if self.t.kind == TokenKind.Str:
                        abi = self.t.raw(self.src)
                        self.advance()
                    yield self.parse_fn(is_extern=True, abi=abi)
                case TokenKind.Fn:
                    yield self.parse_fn()
                case _:
                    panic(f"{self.t.kind} not implemented")


