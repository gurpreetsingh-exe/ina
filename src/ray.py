#!/usr/bin/python

from copy import deepcopy
from enum import Enum, auto
import sys
from beeprint import pp

class TokenKind(Enum):
    Int = auto()
    Float = auto()
    Bool = auto()
    Str  = auto()

    Intrinsic   = auto()
    Ident       = auto()

    Mut         = auto()
    Imm         = auto()
    Let         = auto()
    Extern      = auto()
    Fn          = auto()
    If          = auto()
    Else        = auto()
    As          = auto()

    PLUS        = auto()  # `+`
    MINUS       = auto()  # `-`
    STAR        = auto()  # `*`
    SLASH       = auto()  # `/`
    EQ          = auto()  # `=`
    SEMI        = auto()  # `;`
    LT          = auto()  # `<`
    GT          = auto()  # `>`
    COLON       = auto()  # `:`
    LPAREN      = auto()  # `(`
    RPAREN      = auto()  # `)`
    LCURLY      = auto()  # `{`
    RCURLY      = auto()  # `}`
    LBRACKET    = auto()  # `[`
    RBRACKET    = auto()  # `]`
    COMMA       = auto()  # `,`
    DOUBLEQUOTE = auto()  # `"`
    POUND       = auto()  # `#`
    AT          = auto()  # `@`
    AMPERSAND   = auto()  # `&`
    PIPE        = auto()  # `|`
    TILDE       = auto()  # `~`
    BANG        = auto()  # `!`
    PERCENT     = auto()  # `%`
    DOT         = auto()  # `.`

    LT2         = auto()  # `<<`
    GT2         = auto()  # `>>`
    PIPE2       = auto()  # `||`
    AMPERSAND2  = auto()  # `&&`
    EQ2         = auto()  # `==`
    BANGEQ      = auto()  # `!=`
    ARROW       = auto()  # `->`

    DOT3        = auto()  # `...`

    EOF         = auto()
    UNDEFINED   = auto()

Keywords = {
    "mut"   : TokenKind.Mut,
    "imm"   : TokenKind.Imm,
    "extern": TokenKind.Extern,
    "fn"    : TokenKind.Fn,
    "let"   : TokenKind.Let,
    "if"    : TokenKind.If,
    "else"  : TokenKind.Else,
    "as"    : TokenKind.As,
}

Punctuators = {
    '+' : TokenKind.PLUS,
    '-' : TokenKind.MINUS,
    '*' : TokenKind.STAR,
    '/' : TokenKind.SLASH,
    '=' : TokenKind.EQ,
    ';' : TokenKind.SEMI,
    '<' : TokenKind.LT,
    '>' : TokenKind.GT,
    ':' : TokenKind.COLON,
    '(' : TokenKind.LPAREN,
    ')' : TokenKind.RPAREN,
    '{' : TokenKind.LCURLY,
    '}' : TokenKind.RCURLY,
    '[' : TokenKind.LBRACKET,
    ']' : TokenKind.RBRACKET,
    ',' : TokenKind.COMMA,
    '"' : TokenKind.DOUBLEQUOTE,
    '#' : TokenKind.POUND,
    '@' : TokenKind.AT,
    '&' : TokenKind.AMPERSAND,
    '|' : TokenKind.PIPE,
    '~' : TokenKind.TILDE,
    '!' : TokenKind.BANG,
    '%' : TokenKind.PERCENT,
    '.' : TokenKind.DOT,
    '<<': TokenKind.LT2,
    '>>': TokenKind.GT2,
    '&&': TokenKind.AMPERSAND2,
    '||': TokenKind.PIPE2,
    '==': TokenKind.EQ2,
    '!=': TokenKind.BANGEQ,
    '->': TokenKind.ARROW,
    '...': TokenKind.DOT3,
}


def get_token_name(ty):
    match ty:
        case TokenKind.Ident: return "identifier"
        case TokenKind.Int: return "int"
        case TokenKind.Float: return "float"
        case TokenKind.Bool: return "bool"
        case TokenKind.Str: return "str"
        case TokenKind.Intrinsic: return "intrinsic"
        case TokenKind.Extern: return "extern"
        case TokenKind.Fn: return "fn"
        case TokenKind.Mut: return "mut"
        case TokenKind.Imm: return "imm"
        case TokenKind.Let: return "let"
        case TokenKind.If: return "if"
        case TokenKind.Else: return "else"
        case TokenKind.As: return "as"
        case TokenKind.PLUS: return "+"
        case TokenKind.MINUS: return "-"
        case TokenKind.STAR: return "*"
        case TokenKind.SLASH: return "/"
        case TokenKind.EQ: return "="
        case TokenKind.SEMI: return ";"
        case TokenKind.LT: return "<"
        case TokenKind.GT: return ">"
        case TokenKind.COLON: return ":"
        case TokenKind.LPAREN: return "("
        case TokenKind.RPAREN: return ")"
        case TokenKind.LCURLY: return "{"
        case TokenKind.RCURLY: return "}"
        case TokenKind.LBRACKET: return "["
        case TokenKind.RBRACKET: return "]"
        case TokenKind.COMMA: return ","
        case TokenKind.DOUBLEQUOTE: return '"'
        case TokenKind.POUND: return "#"
        case TokenKind.AT: return "@"
        case TokenKind.AMPERSAND: return "&"
        case TokenKind.PIPE: return "|"
        case TokenKind.TILDE: return "~"
        case TokenKind.BANG: return "!"
        case TokenKind.PERCENT: return "%"
        case TokenKind.DOT: return "."
        case TokenKind.LT2: return "<<"
        case TokenKind.GT2: return ">>"
        case TokenKind.PIPE2: return "||"
        case TokenKind.AMPERSAND2: return "&&"
        case TokenKind.EQ2: return "=="
        case TokenKind.BANGEQ: return "!="
        case TokenKind.ARROW: return "->"
        case TokenKind.DOT3: return "..."


class Token:
    __match_args__ = ('kind', 'loc', )
    def __init__(self, kind, loc):
        self.kind = kind
        self.loc = loc

    def raw(self, src):
        loc = self.loc
        return src[loc.id:loc.id + loc.size]

    def __repr__(self):
        return f"{str(self.kind).ljust(16)}\t=> `{str(self.loc)}`"

class Loc:
    def __init__(self, id, size, file=""):
        self.id = id
        self.line = 0
        self.col = 0
        self.size = size
        self.file = file

    def fmt(self):
        return f"{self.line + 1}:{self.col + 1}"

    def __repr__(self) -> str:
        return f"{self.fmt()}: {self.id}: {self.size}"

def lexer_from_file(filepath):
    with open(filepath, "r") as f:
        program = f.read()
        return lexer_from_src(program, filepath)

def lexer_from_src(src, filepath):
    return Lexer(src, filepath)

class Lexer:
    def __init__(self, program, filepath):
        self.program = program
        self.loc = Loc(0, 0, filepath)
        self.curr_char = self.program[self.loc.id]

    def advance(self):
        self.loc.id += 1
        if self.curr_char == '\n':
            self.loc.col = 0
            self.loc.line += 1
        else:
            self.loc.col += 1
        self.curr_char = self.program[self.loc.id] if self.loc.id < len(self.program) else TokenKind.EOF

    def lex_word(self, method):
        buffer = ''

        while self.curr_char and method(self) and (not self.curr_char.isspace()):
            buffer += self.curr_char
            self.advance()

        return buffer

    def lexfile(self):
        while self.curr_char:
            loc = deepcopy(self.loc)

            if self.curr_char == TokenKind.EOF:
                yield Token(self.curr_char, loc)
                break

            if self.curr_char.isspace():
                self.advance()
                continue

            elif self.curr_char.isdigit():
                word = self.lex_word(lambda self: self.curr_char.isdigit() or self.curr_char == ".")
                loc.size = self.loc.id - loc.id
                try:
                    int(word)
                    typ = TokenKind.Int
                except ValueError:
                    float(word)
                    typ = TokenKind.Float

                yield Token(typ, loc)

            elif self.curr_char == "\"":
                buf = ""
                self.advance()
                while self.curr_char != "\"":
                    buf += self.curr_char
                    self.advance()
                self.advance()
                loc.size = self.loc.id - loc.id
                yield Token(TokenKind.Str, loc)

            elif self.curr_char.isalpha() or self.curr_char == "_":
                word = self.lex_word(lambda self: self.curr_char.isalnum() or self.curr_char == "_")
                loc.size = self.loc.id - loc.id

                if word in Keywords:
                    yield Token(Keywords[word], loc)
                elif word in {'true', 'false'}:
                    yield Token(TokenKind.Bool, loc)
                else:
                    yield Token(TokenKind.Ident, loc)

            elif self.curr_char in Punctuators:
                prev = self.curr_char
                self.advance()
                compound = prev + self.curr_char

                if compound == "//":
                    while self.curr_char != "\n":
                        self.advance()
                    continue
                elif compound == "..":
                    self.advance()
                    if self.curr_char == ".":
                        loc.size = self.loc.id - loc.id
                        yield Token(Punctuators["..."], loc)
                        self.advance()
                    else:
                        assert False, "Unexpected token"
                elif compound in Punctuators:
                    loc.size = self.loc.id - loc.id
                    yield Token(Punctuators[compound], loc)
                    self.advance()
                else:
                    loc.size = self.loc.id - loc.id
                    yield Token(Punctuators[prev], loc)

            else:
                assert False, "unreachable"


# AST

symtab = {
    'fn': 1,
    'mut': 2,
    'imm': 3,
    'extern': 4,
    'int': 5,
    'bool': 6,
    'str': 7,
    'char': 8,
    'float': 9,
    'raw': 10,
}

def get_prim_ty(ty):
    match ty:
        case 'int': return TyKind.Int
        case 'float': return TyKind.Float
        case 'bool': return TyKind.Bool
        case 'str': return TyKind.Str
        case 'char': return TyKind.Char
        case 'raw': return TyKind.Raw
        case _: panic("unexpected type")

errors = []

def panic(msg):
    errors.append(msg + "\n")
    assert False, msg

class TyKind(Enum):
    Int   = auto()
    Float = auto()
    Bool  = auto()
    Str   = auto()
    Char  = auto()
    Raw   = auto()


class Ty:
    def __init__(self, ty):
        self.ty = ty

    def get_size(self):
        match self.ty:
            case TyKind.Int | TyKind.Float:
                return 8
            case TyKind.Str:
                return 8
            case TyKind.Raw:
                return 8
            case TyKind.Bool:
                return 8
            case _:
                assert False, "type not found"

    def __eq__(self, __o: object) -> bool:
        return self.ty == __o.ty

    def __ne__(self, __o: object) -> bool:
        return not self.__eq__(__o)

    def __repr__(self):
        return str(self.ty)

class Arg:
    def __init__(self, name, ty):
        self.name = name
        self.ty = ty

class Variadic:
    def __init__(self):
        pass

class Fn:
    __match_args__ = ("name", "args", "ret_ty", "body", "is_extern", "abi")
    def __init__(self, name, args, ret_ty, body, is_extern, abi):
        self.name = name
        self.args = args
        self.ret_ty = ret_ty
        self.body = body
        self.is_extern = is_extern
        self.abi = abi

        self.alignment = ((self.local_count + 1) // 2) * 16

    @property
    def stack_offset(self):
        stack_off = 0
        for arg in self.args:
            if type(arg) == Variadic:
                continue
            stack_off += arg.ty.get_size()
        if not self.body:
            return stack_off
        for node in self.body.stmts:
            if type(node) == Let:
                stack_off += node.ty.get_size()
        return stack_off

    @property
    def stack_alignment(self):
        return (self.stack_offset + 15) & ~15

    @property
    def local_count(self):
        local_count_ = len(self.args)
        if not self.body:
            return local_count_
        for node in self.body.stmts:
            if type(node) == Let:
                local_count_ += 1
        return local_count_

class Block:
    def __init__(self, stmts):
        self.stmts = stmts

class Binary:
    __match_args__ = ("op", "left", "right")
    def __init__(self, op, left, right):
        self.op = op
        self.left = left
        self.right = right
        self.ty = None

class Unary:
    __match_args__ = ("op", "expr", )
    def __init__(self, op, expr):
        self.op = op
        self.expr = expr
        self.ty = None

class Call:
    __match_args__ = ("name", "args")
    def __init__(self, name, args):
        self.name = name
        self.args = args
        self.ty = None

class If:
    def __init__(self, cond, body, elze = None):
        self.cond = cond
        self.body = body
        self.elze = elze
        self.ty = None

class Else:
    def __init__(self, body):
        self.body = body

class Let:
    __match_args__ = ("name", "ty", "init")
    def __init__(self, name, ty, init):
        self.name = name
        self.ty = ty
        self.init = init

class Assign:
    def __init__(self, name, init):
        self.name = name
        self.init = init

class Ident:
    __match_args__ = ("name", )
    def __init__(self, name):
        self.name = name
        self.ty = None

class Cast:
    __match_args__ = ("expr", "ty", )
    def __init__(self, expr, ty):
        self.expr = expr
        self.ty = ty


class Lit(Enum):
    Int   = auto()
    Float = auto()
    Bool  = auto()
    Str   = auto()
    Char  = auto()

class Literal:
    __match_args__ = ("kind", "value")
    def __init__(self, kind, value):
        self.kind = kind
        self.value = value
        self.ty = None


class Parser:
    def __init__(self, src, tokens):
        self.tokens = tokens
        self.src = src
        self.id = -1
        self.prev = None
        self.t = None

    def check(self):
        return self.id < len(self.tokens) - 1

    def advance(self):
        if self.check():
            if self.id >= 0:
                self.prev = self.tokens[self.id]
            self.id += 1
            self.t = self.tokens[self.id]

    def eat_if_present(self, kind):
        present = self.t.kind == kind
        if present:
            self.advance()
        return present

    def peek(self):
        if self.check():
            return self.tokens[self.id + 1]
        else:
            panic("End of stream")

    def expect(self, kind):
        if self.t.kind == kind:
            curr = self.t
            self.advance()
            return curr
        else:
            panic(f"expected `{kind}` got `{self.t.kind}` ({self.t.raw(self.src)})")

    def parse_ty(self):
        match self.t.kind:
            case TokenKind.Ident:
                ty = self.expect(TokenKind.Ident).raw(self.src)
                if prim_ty := get_prim_ty(ty):
                    return Ty(prim_ty)
                else:
                    return Ty(ty)
            case _:
                panic("unexpected type")

    def parse_arg(self):
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

    def parse_fn_args(self):
        self.expect(TokenKind.LPAREN)

        while self.check() and self.t.kind != TokenKind.RPAREN:
            yield self.parse_arg()
            if self.t.kind == TokenKind.COMMA:
                self.advance()

        self.expect(TokenKind.RPAREN)

    def parse_ret_ty(self):
        if self.t.kind == TokenKind.ARROW:
            self.advance()
            return self.parse_ty()

    def compound(self, tok_list, callback):
        left = callback()
        while self.check() and self.t.kind in tok_list:
            op = self.t
            self.advance()
            right = callback()
            left = Binary(op, left, right)

        return left

    def token_to_lit(self):
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

    def parse_call(self):
        name = self.expect(TokenKind.Ident).raw(self.src)
        self.expect(TokenKind.LPAREN)
        args = []
        while self.check() and self.t.kind != TokenKind.RPAREN:
            args.append(self.parse_expr())
            if self.t.kind == TokenKind.COMMA:
                self.advance()
        self.expect(TokenKind.RPAREN)
        return Call(name, args)

    def parse_primary(self):
        match self.t.kind:
            case TokenKind.Ident:
                next_ = self.peek()
                if next_.kind == TokenKind.LPAREN:
                    return self.parse_call()
                else:
                    return Ident(self.expect(TokenKind.Ident).raw(self.src))
            case TokenKind.Int | TokenKind.Float | TokenKind.Bool | TokenKind.Str:
                value = self.t.raw(self.src)
                lit_kind = self.token_to_lit()
                self.advance()
                return Literal(lit_kind, value)
            case TokenKind.If:
                self.advance()
                cond = self.parse_expr()
                body = self.parse_block()
                elze = None
                if self.t.kind == TokenKind.Else:
                    self.advance()
                    elze = self.parse_block()
                return If(cond, body, elze)
            case _:
                panic(f"unreachable {self.t.kind}")

    def parse_unary(self):
        if self.t.kind in [TokenKind.MINUS, TokenKind.BANG, TokenKind.AMPERSAND]:
            op = self.t
            self.advance()
            right = self.parse_unary()
            return Unary(op, right)

        return self.parse_primary()

    def parse_factor(self):
        prim = self.compound([TokenKind.STAR, TokenKind.SLASH], self.parse_unary)
        if self.t.kind == TokenKind.As:
            self.advance()
            ty = self.parse_ty()
            prim = Cast(prim, ty)
        return prim

    def parse_term(self):
        return self.compound([TokenKind.PLUS, TokenKind.MINUS], self.parse_factor)

    def parse_comparison(self):
        return self.compound([TokenKind.LT, TokenKind.GT], self.parse_term)

    def parse_assign(self):
        left = self.parse_comparison()
        while self.t.kind == TokenKind.EQ:
            self.advance()
            init = self.parse_assign()
            if isinstance(left, Ident):
                return Assign(left.name, init)
            else:
                panic(f"Assignment expression expected `Ident` but got `{left}`")
        return left

    def parse_expr(self):
        return self.compound([TokenKind.BANGEQ, TokenKind.EQ2], self.parse_assign)

    def parse_let(self):
        self.expect(TokenKind.Let)
        name = self.expect(TokenKind.Ident).raw(self.src)
        ty = None
        if self.t.kind == TokenKind.COLON:
            self.advance()
            ty = self.parse_ty()
        self.expect(TokenKind.EQ)
        init = self.parse_expr()
        return Let(name, ty, init)

    def parse_stmt(self):
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
        return stmt

    def parse_block(self):
        self.expect(TokenKind.LCURLY)
        stmts = []
        while self.check() and self.t.kind != TokenKind.RCURLY:
            stmts.append(self.parse_stmt())
        self.expect(TokenKind.RCURLY)
        return Block(stmts)

    def parse_fn(self, is_extern=False, abi=None):
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


class DefKind(Enum):
    Fn = auto()
    Struct = auto()

class Def:
    def __init__(self, kind, data):
        self.kind = kind
        self.data = data

class TyScope:
    def __init__(self):
        self.scopes = []

    @property
    def stack_offset(self):
        self.scopes[-1]

    def search_local(self, name):
        for scope in reversed(self.scopes):
            for var in scope:
                if var['name'] == name:
                    return True
        return False
        # return any([True for local in self.scopes[-1] if local['name'] == name])

    def find_local(self, name):
        for scope in reversed(self.scopes):
            for var in scope:
                if var['name'] == name:
                    return var['ty']

    def def_local(self, name, ty, val=None):
        if self.search_local(name):
            panic(f"{name} is already defined")
        self.scopes[-1].append({ 'name': name, 'ty': ty, 'val': val })

    def add(self):
        self.scopes.append([])

    def pop(self):
        self.scopes.pop()

class TyCheck:
    def __init__(self, ast):
        self.ast = ast
        self.defs = {}
        self.scope = TyScope()
        self.fn_ctx = None
        self.tychk()

    def infer(self, expr):
        match expr:
            case Binary(op, left, right):
                lty = self.infer(left)
                rty = self.infer(right)
                if lty == rty:
                    match op:
                        case Token(TokenKind.LT | TokenKind.GT, _):
                            return Ty(TyKind.Bool)
                    return lty
                else:
                    panic(f"{op} is not implemented for {lty} and {rty}")
            case Literal(kind, _):
                val = None
                match kind:
                    case Lit.Int:
                        val = TyKind.Int
                    case Lit.Float:
                        val = TyKind.Float
                    case Lit.Bool:
                        val = TyKind.Bool
                    case Lit.Str:
                        val = TyKind.Str
                    case Lit.Char:
                        val = TyKind.Char
                    case _:
                        panic(f"unexpected literal {kind}")
                ty = Ty(val)
                expr.ty = ty
                return ty
            case Call(name, args):
                if ty := self.check_call(name, args):
                    return ty
                else:
                    panic("return type expected")
            case Ident(name):
                if not self.scope.search_local(name):
                    panic(f"{name} is not defined")
                ty = self.scope.find_local(name)
                expr.ty = ty
                return ty
            case Unary(op, expr):
                match op.kind:
                    case TokenKind.BANG:
                        if (ty := self.infer(expr).ty) != TyKind.Bool:
                            panic(f"expected bool but got {ty}")
                        return Ty(TyKind.Bool)
                    case TokenKind.MINUS:
                        if (ty := self.infer(expr).ty) not in [TyKind.Int, TyKind.Float]:
                            panic(f"expected number but got {ty}")
                        return Ty(ty)
                    case TokenKind.AMPERSAND:
                        if type(expr) != Ident:
                            panic(f"cannot use & on {expr.__class__.__name__}")
                        return Ty(TyKind.Raw)
                    case _:
                        assert False, f"{op} unreachable"
            case If():
                self.check(expr.cond, Ty(TyKind.Bool))
                if_ty = self.infer_block(expr.body)
                else_ty = None
                if expr.elze:
                    else_ty = self.infer_block(expr.elze)
                return if_ty
            case Cast(expr, ty):
                expr.ty = self.infer(expr)
                return ty
            case _:
                assert False

    def check(self, expr, expected_ty):
        match expr:
            case Binary(op, left, right):
                match op:
                    case Token(TokenKind.LT | TokenKind.GT, _):
                        if expected_ty.ty != TyKind.Bool:
                            panic(f"expected {expected_ty.ty} but got bool")
                        lty = self.infer(left)
                        rty = self.infer(right)
                    case _:
                        self.check(left, expected_ty)
                        self.check(right, expected_ty)
            case Literal(kind, _):
                val = None
                match kind:
                    case Lit.Int:
                        val = TyKind.Int
                    case Lit.Float:
                        val = TyKind.Float
                    case Lit.Bool:
                        val = TyKind.Bool
                    case Lit.Str:
                        val = TyKind.Str
                    case Lit.Char:
                        val = TyKind.Char
                    case _:
                        panic(f"unexpected literal {kind}")
                ty = Ty(val)
                if expected_ty and ty != expected_ty:
                    panic(f"expected {expected_ty} but got {ty}")
            case Call(name, args):
                self.check_call(name, args, expected_ty)
            case Ident(name):
                if ty := self.scope.find_local(name):
                    if ty != expected_ty:
                        panic(f"expected {expected_ty}, found {ty}")
                else:
                    panic(f"{name} is not defined")
            case Unary(op, expr):
                match op.kind:
                    case TokenKind.BANG:
                        if expected_ty.ty != TyKind.Bool:
                            panic(f"expected {expected_ty} but got Bool")
                        self.check(expr, expected_ty)
                    case TokenKind.MINUS:
                        if expected_ty.ty not in [TyKind.Int, TyKind.Float]:
                            panic(f"expected {expected_ty} but got number")
                        self.check(expr, expected_ty)
                    case TokenKind.AMPERSAND:
                        if type(expr) != Ident:
                            panic(f"cannot use & on {expr.__class__.__name__}")
                        if expected_ty.ty != TyKind.Raw:
                            panic(f"expected {expected_ty} but got Raw")
                    case _:
                        assert False, f"{op} unreachable"
            case If():
                self.check(expr.cond, Ty(TyKind.Bool))
                self.check_block(expr.body)
                if expr.elze:
                    self.check_block(expr.elze)
            case Cast(expr, ty):
                expr.ty = self.infer(expr)
                return ty
            case _:
                assert False

    def check_call(self, name, args, expected_ty=None):
        defn = self.defs.get(name)
        if not defn:
            panic(f"{name} is not defined")

        variadic = False
        starts_at = 0
        for arg in defn.data['args']:
            if type(arg) == Variadic:
                variadic = True
                break
            starts_at += 1

        found = len(args)
        expected = len(defn.data['args'])

        if not variadic and found != expected:
            panic(f"expected {expected} args found {found}")

        if variadic:
            exp = defn.data['args']
            for i, arg in enumerate(args):
                if i >= starts_at:
                    arg.ty = self.infer(arg)
                else:
                    self.check(arg, exp[i].ty)
                    arg.ty = exp[i].ty
        else:
            for arg, exp in zip(args, defn.data['args']):
                self.check(arg, exp.ty)
                arg.ty = exp.ty

        ty = defn.data['ret_ty']
        if expected_ty and ty != expected_ty:
            panic(f"unexpected type: expected {expected_ty} found {ty}")
        return ty

    def check_stmt(self, stmt, expected_ty):
        match stmt:
            case Let(name, ty, init):
                if not ty:
                    ty = self.infer(init)
                    stmt.ty = ty
                    init.ty = ty
                else:
                    self.check(init, ty)
                    init.ty = ty
                self.scope.def_local(name, ty)
            case _:
                self.check(stmt, expected_ty)
                stmt.ty = expected_ty
                return expected_ty

    def infer_stmt(self, stmt):
        match stmt:
            case Let(name, ty, init):
                if not ty:
                    ty = self.infer(init)
                    stmt.ty = ty
                    init.ty = ty
                else:
                    self.check(init, ty)
                    init.ty = ty
                self.scope.def_local(name, ty)
            case _:
                ty = self.infer(stmt)
                stmt.ty = ty
                return ty

    def check_block(self, block):
        self.scope.add()
        ret_ty = None
        if self.fn_ctx:
            deff = self.defs[self.fn_ctx]
            assert deff.kind == DefKind.Fn
            fn_dict = deff.data
            for arg in fn_dict['args']:
                if type(arg) == Variadic:
                    continue
                self.scope.def_local(arg.name, arg.ty)
            ret_ty = fn_dict['ret_ty']
        self.fn_ctx = None
        for stmt in block.stmts:
            self.check_stmt(stmt, ret_ty)
        self.scope.pop()

    def infer_block(self, block):
        self.scope.add()
        if self.fn_ctx:
            deff = self.defs[self.fn_ctx]
            assert deff.kind == DefKind.Fn
            fn_dict = deff.data
            for arg in fn_dict['args']:
                if type(arg) == Variadic:
                    continue
                self.scope.def_local(arg.name, arg.ty)
        self.fn_ctx = None
        tys = []
        for stmt in block.stmts:
            tys.append(self.infer_stmt(stmt))
        self.scope.pop()
        if tys:
            return tys[-1]

    def tychk(self):
        for node in self.ast:
            match node:
                case Fn(name, args, ret_ty, body, _, abi):
                    fn_dict = { 'args': args, 'ret_ty': ret_ty, 'abi': abi }
                    self.defs[name] = Def(DefKind.Fn, fn_dict)
                    self.fn_ctx = name
                    if body:
                        self.check_block(body)
                case _:
                    assert False, f"{node}"


regs = ["rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
fn_regs = [
    ["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
    ["xmm0", "xmm1"]
]

class Scope:
    def __init__(self):
        self.scopes = []
        self.stack_offset = 0

    def search_local(self, name):
        return any([True for local in self.scopes[-1] if local['name'] == name])

    def def_local(self, name, ty):
        if self.search_local(name):
            panic(f"{name} is already defined")
        size = 0
        size = ty.get_size()

        self.stack_offset += size
        self.scopes[-1].append({ 'name': name, 'off': self.stack_offset })
        return self.stack_offset

    def find_local(self, name):
        for scope in reversed(self.scopes):
            for var in scope:
                if var['name'] == name:
                    return var['off']

    def add(self):
        self.scopes.append([])

    def pop(self):
        self.scopes.pop()
        self.stack_offset = 0

class Codegen:
    def __init__(self, ast, defs):
        self.ast = ast
        self.buf = ""
        self.scopes = Scope()
        self.strings = []
        self.floats = []
        self.defs = defs
        self.label_id = 1

    @property
    def label(self):
        i = self.label_id
        self.label_id += 1
        return i

    def append(self, expr, stack):
        match expr:
            case Binary(op, left, right):
                stack += self.binop_stack(left)
            case _:
                stack.append(expr)

    def binop_stack(self, binary):
        stack = []
        match binary:
            case Binary(_):
                self.append(binary.left, stack)
                self.append(binary.right, stack)
                stack.append(binary.op)
            case _:
                stack.append(binary)
        return stack

    def expr(self, expr, reg, label = 0):
        match expr:
            case Binary():
                stack = self.binop_stack(expr)
                stack0 = []
                for i, expr in enumerate(stack):
                    match expr:
                        case Token(kind, _):
                            b = stack0.pop()
                            a = stack0.pop()
                            match kind:
                                case TokenKind.PLUS:
                                    self.buf += f"    add {a}, {b}\n"
                                case TokenKind.MINUS:
                                    self.buf += f"    sub {a}, {b}\n"
                                case TokenKind.LT | TokenKind.GT:
                                    self.buf += f"    cmp {a}, {b}\n"
                                    if reg:
                                        setT = "setl" if kind == TokenKind.LT else "setg"
                                        self.buf += f"    {setT} al\n"
                                        if reg != "rax":
                                            self.buf += f"    movzx {a}, al\n"
                                    else:
                                        if label:
                                            jmp = "jge" if kind == TokenKind.LT else "jle"
                                            self.buf += f"    {jmp} .L{label}\n"
                                case _:
                                    assert False, f"{kind} not implemented"
                            stack0.append(a)
                        case _:
                            self.expr(expr, regs[i])
                            stack0.append(regs[i])
                if reg and reg != "rax":
                    self.buf += f"    mov {reg}, rax\n"
            case Ident(name):
                off = self.scopes.find_local(name)
                if reg:
                    if reg in fn_regs[1]:
                        self.buf += f"    mov rax, [rbp - {off}]\n"
                        self.buf += f"    movq {reg}, rax\n"
                    else:
                        self.buf += f"    mov {reg}, [rbp - {off}]\n"
                else:
                    self.buf += f"    mov rax, [rbp - {off}]\n"
            case Call(name, args):
                defn = self.defs.get(name)
                variadic = False
                for arg in defn.data['args']:
                    if type(arg) == Variadic:
                        variadic = True
                        break

                i = 0
                for arg in args:
                    if arg.ty.ty == TyKind.Float:
                        continue
                    self.expr(arg, fn_regs[0][i])
                    i += 1

                flt_args = 0
                i = 0
                for arg in args:
                    if is_float := arg.ty.ty == TyKind.Float:
                        flt_args += is_float
                        self.expr(arg, fn_regs[1][i])
                        i += 1

                if defn.data['abi']:
                    match abi := defn.data['abi'][1:-1], variadic:
                        case "C", True:
                            if flt_args:
                                self.buf += f"    mov rax, {flt_args}\n"
                            else:
                                self.buf += "    xor rax, rax\n"
                        case "C", False:
                            pass
                        case _:
                            assert False, f"{abi} abi is not implemented"
                self.buf += f"    call {name}\n"
                if defn.data['ret_ty'] and reg and reg != "rax":
                    self.buf += f"    mov {reg}, rax\n"
            case Literal(kind, value):
                if not reg:
                    return
                match kind:
                    case Lit.Str:
                        self.buf += f"    mov {reg}, OFFSET FLAT:str{len(self.strings)}\n"
                        self.strings.append(value)
                    case Lit.Bool:
                        self.buf += f"    mov {reg}, {1 if value == 'true' else 0}\n"
                    case Lit.Float:
                        label = self.label
                        self.buf += f"    movsd {reg}, QWORD PTR .L{label}[rip]\n"
                        self.floats.append((label, value))
                    case _:
                        self.buf += f"    mov {reg}, {value}\n"
            case Unary(op, expr):
                if not reg:
                    match op.kind:
                        case TokenKind.AMPERSAND:
                            assert False
                        case TokenKind.BANG:
                            self.buf += f"    mov rbx, 1\n"
                            self.expr(expr, None)
                            self.buf += f"    sub rbx, rax\n"
                            self.buf += f"    mov rax, rbx\n"
                        case TokenKind.MINUS:
                            self.buf += f"    mov rbx, 0\n"
                            self.expr(expr, None)
                            self.buf += f"    sub rbx, rax\n"
                            self.buf += f"    mov rax, rbx\n"
                        case _:
                            assert False, "not implemented"
                    return
                match op.kind:
                    case TokenKind.AMPERSAND:
                        off = self.scopes.find_local(expr.name)
                        self.buf += f"    lea {reg}, [rbp - {off}]\n"
                    case TokenKind.BANG:
                        self.buf += f"    mov rbx, 1\n"
                        self.expr(expr, reg)
                        self.buf += f"    sub {reg}, rbx\n"
                    case TokenKind.MINUS:
                        self.buf += f"    mov rax, 0\n"
                        self.expr(expr, "rbx")
                        self.buf += f"    sub rax, rbx\n"
                    case _:
                        assert False, "not implemented"
            case If():
                has_else = bool(expr.elze)
                label_false = self.label
                label_end = None
                if has_else:
                    label_end = self.label
                self.expr(expr.cond, None, label_false)
                match expr.cond:
                    case Literal():
                        if expr.cond.value == 'false':
                            self.buf += f"    jmp .L{label_false}\n"
                    case Ident() | Unary() | Call():
                        self.buf += f"    cmp rax, 0\n"
                        self.buf += f"    je .L{label_false}\n"
                self.gen(expr.body.stmts)
                if has_else:
                    self.buf += f"    jmp .L{label_end}\n"
                self.buf += f".L{label_false}:\n"
                if has_else:
                    self.gen(expr.elze.stmts)
                    self.buf += f".L{label_end}:\n"
                if reg and reg != "rax":
                    self.buf += f"    mov {reg}, rax\n"
            case Cast(expr, ty):
                if expr.ty == ty:
                    self.expr(expr, reg)
                    return
                match ty.ty:
                    case TyKind.Float:
                        self.expr(expr, reg)
                        self.buf += f"    cvtsi2sd xmm0, {reg}\n"
                        self.buf += f"    movq {reg}, xmm0\n"
                    case TyKind.Int:
                        self.expr(expr, "xmm0")
                        self.buf += f"    cvttsd2si {reg}, xmm0\n"
                    case _:
                        assert False
            case _:
                assert False, f"{type(expr)} is not implemented"

    def gen(self, nodes):
        self.scopes.add()
        for node in nodes:
            match node:
                case Fn(name, args, ret_ty, body, is_extern, _):
                    if is_extern:
                        continue
                    self.buf += f"{name}:\n    push rbp\n    mov rbp, rsp\n"
                    alignment = node.stack_alignment
                    if alignment:
                        self.buf += f"    sub rsp, {alignment}\n"
                    for i, arg in enumerate(args):
                        if type(arg) == Variadic:
                            continue
                        off = self.scopes.def_local(arg.name, arg.ty)
                        is_float = arg.ty.ty == TyKind.Float
                        self.buf += f"    mov [rbp - {off}], {fn_regs[is_float][i]}\n"
                    self.gen(body.stmts)
                    if alignment:
                        self.buf += f"    add rsp, {alignment}\n"
                    if not ret_ty:
                        self.buf += f"    xor rax, rax\n"
                    self.buf += "    pop rbp\n    ret\n"
                case Let(name, ty, init):
                    off = self.scopes.def_local(name, ty)
                    if type(init) == Literal:
                        match init.kind:
                            case Lit.Str:
                                self.expr(init, "rax")
                                self.buf += f"    mov [rbp - {off}], rax\n"
                            case Lit.Int:
                                self.buf += f"    mov QWORD PTR [rbp - {off}], {init.value}\n"
                            case Lit.Bool:
                                self.buf += f"    mov QWORD PTR [rbp - {off}], {1 if init.value == 'true' else 0}\n"
                            case Lit.Float:
                                self.expr(init, "xmm0")
                                self.buf += f"    movsd [rbp - {off}], xmm0\n"
                            case _:
                                assert False
                    else:
                        self.expr(init, "rax")
                        self.buf += f"    mov [rbp - {off}], rax\n"
                case Binary() | Call() | Unary() | If() | Literal():
                    self.expr(node, "rax")
                case _:
                    assert False, f"{type(node)} is not implemented"
        self.scopes.pop()

    def emit_data(self):
        import struct
        self.buf += ".data\n"
        for i, string in enumerate(self.strings):
            self.buf += f"str{i}:\n"
            self.buf += f"    .string {string}\n"
        for i, flt in self.floats:
            self.buf += f".L{i}:\n"
            flt = struct.pack('d', float(flt))
            hi = int.from_bytes(flt[:4], 'little')
            lo = int.from_bytes(flt[4:], 'little')
            self.buf += f"    .long {hi}\n    .long {lo}\n"

    def emit(self):
        self.buf += ".intel_syntax noprefix\n.text\n.globl main\n"
        self.gen(self.ast)
        self.emit_data()
        return self.buf


def usage(arg0):
    print(f"Usage: {arg0} [command] [options] input...")
    print("\nCommands:")
    print("    com          compile the file")
    print("\nOptions:")
    print("    -h, --help   print help information\n")
    exit(1)

class Command(Enum):
    Nan = auto()
    Compile = auto()

def main(argv):
    filename = ""
    arg0 = argv[0]
    argv = argv[1:]
    command = Command.Nan
    while argv:
        arg = argv[0]
        if arg.startswith("-"):
            if len(arg) < 2:
                usage(arg0)
            match arg[1]:
                case '-':
                    match arg:
                        case "--help":
                            usage(arg0)
                        case _:
                            print(f"Unknown option \"{arg}\"")
                            usage(arg0)
                case 'h':
                    usage(arg0)
                case _:
                    print(f"Unknown option \"{arg}\"")
                    usage(arg0)
        elif command == Command.Nan:
            match arg:
                case "com":
                    command = Command.Compile
                case _:
                    print(f"Unknown command \"{arg}\"")
        else:
            match command:
                case Command.Nan:
                    usage(arg0)
                case _:
                    if not filename:
                        filename = arg
                    else:
                        usage(arg0)
        argv = argv[1:]

    match command:
        case Command.Nan:
            usage(arg0)
        case _:
            if not filename:
                usage(arg0)
            match command:
                case Command.Compile:
                    lexer = lexer_from_file(filename)
                    src = lexer.program
                    tokens = list(lexer.lexfile())
                    parser = Parser(src, tokens)
                    ast = list(parser.parse())
                    tychk = TyCheck(ast)
                    # gen = IRGen(ast)
                    # pp(ast, max_depth=10)
                    code = Codegen(ast, tychk.defs).emit()
                    output = filename.split('.')[0]
                    with open(f"{output}.asm", "w") as f:
                        f.write(code)

                    from subprocess import call
                    call(["as", f"{output}.asm", "-o", f"{output}.o"])
                    call(["gcc", f"{output}.o", "-o", output])
                case _:
                    assert False, "unreachable"


if __name__ == "__main__":
    main(sys.argv)
