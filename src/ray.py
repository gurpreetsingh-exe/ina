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
    program = ""
    with open(filepath, "r") as f:
        program = f.read()
    return Lexer(program, filepath)

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

def panic(msg):
    print(msg)
    exit(1)

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

class Unary:
    def __init__(self, op, expr):
        self.op = op
        self.expr = expr

class Call:
    __match_args__ = ("name", "args")
    def __init__(self, name, args):
        self.name = name
        self.args = args

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
            case _:
                panic(f"unreachable {self.t.kind}")

    def parse_unary(self):
        if self.t.kind in [TokenKind.MINUS, TokenKind.BANG]:
            op = self.t
            self.advance()
            right = self.parse_unary()
            return Unary(op, right)

        return self.parse_primary()

    def parse_factor(self):
        return self.compound([TokenKind.STAR, TokenKind.SLASH], self.parse_unary)

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
            case TokenKind.LCURLY:
                stmt = self.parse_block()
            case _:
                stmt = self.parse_expr()
        self.expect(TokenKind.SEMI)
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
        return any([True for local in self.scopes[-1] if local['name'] == name])

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
        self.analyze(self.ast)

    def infer(self, expr):
        match expr:
            case Binary(op, left, right):
                lty = self.infer(left)
                rty = self.infer(right)
                if lty == rty:
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
                return Ty(val)
            case Call(name, args):
                if ty := self.check_call(name, args):
                    return ty
                else:
                    panic("return type expected")
            case Ident(name):
                if not self.scope.search_local(name):
                    panic(f"{name} is not defined")
            case _:
                assert False

    def check(self, expr, expected_ty):
        match expr:
            case Binary(op, left, right):
                self.check_binary(expr, expected_ty)
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
                if ty != expected_ty:
                    panic(f"expected {expected_ty} but got {ty}")
            case Call(name, args):
                self.check_call(name, args, expected_ty)
            case _:
                assert False

    def check_call(self, name, args, expected_ty=None):
        defn = self.defs.get(name)
        variadic = False

        for arg in defn.data['args']:
            if type(arg) == Variadic:
                variadic = True
                break

        found = len(args)
        expected = len(defn.data['args'])

        if not variadic and found != expected:
            panic(f"expected {expected} args found {found}")

        for arg, exp in zip(args, defn.data['args']):
            if variadic:
                self.infer(arg)
            else:
                self.check(arg, exp.ty)

        ty = defn.data['ret_ty']
        if expected_ty and ty != expected_ty:
            panic(f"unexpected type: expected {expected_ty} found {ty}")
        return ty

    def check_binary(self, node, expected_ty=None):
        self.check(node.left, expected_ty)
        self.check(node.right, expected_ty)

    def analyze(self, nodes):
        self.scope.add()
        for i, node in enumerate(nodes):
            match node:
                case Fn(name, args, ret_ty, body, _, abi):
                    fn_dict = { 'args': args, 'ret_ty': ret_ty, 'abi': abi }
                    self.defs[name] = Def(DefKind.Fn, fn_dict)
                    if body:
                        self.analyze(body.stmts)
                case Let(name, ty, init):
                    if not ty:
                        ty = self.infer(init)
                        nodes[i].ty = ty
                        self.scope.def_local(name, ty)
                    else:
                        self.check(init, ty)
                case Call(name, args):
                    self.check_call(name, args)
                case Binary(op, left, right):
                    self.check_binary(node)
                case _:
                    assert False, f"{node}"
        self.scope.pop()


class Reg(Enum):
    rax = auto()


regs = ["rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
fn_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

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
        match ty.ty:
            case TyKind.Int | TyKind.Float:
                size = 8
            case TyKind.Str:
                size = 8
            case TyKind.Raw:
                size = 8
            case _:
                assert False, "type not found"

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
        self.defs = defs

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

    def expr(self, expr, reg):
        match expr:
            case Binary():
                stack = self.binop_stack(expr)
                stack0 = []
                for i, expr in enumerate(stack):
                    match expr:
                        case Token():
                            b = stack0.pop()
                            a = stack0.pop()
                            self.buf += f"    add {a}, {b}\n"
                            stack0.append(a)
                        case _:
                            self.expr(expr, regs[i])
                            stack0.append(regs[i])
            case Ident(name):
                off = self.scopes.find_local(name)
                self.buf += f"    mov {reg}, [rbp - {off}]\n"
            case Call(name, args):
                defn = self.defs.get(name)
                variadic = False
                for arg in defn.data['args']:
                    if type(arg) == Variadic:
                        variadic = True
                        break

                for i, arg in enumerate(args):
                    self.expr(arg, fn_regs[i])
                match abi := defn.data['abi'][1:-1], variadic:
                    case "C", True:
                        self.buf += "    xor rax, rax\n"
                    case "C", False:
                        pass
                    case _:
                        assert False, f"{abi} abi is not implemented"
                self.buf += f"    call {name}\n"
                if defn.data['ret_ty'] and reg != "rax":
                    self.buf += f"    mov {reg}, rax\n"
            case Literal(kind, value):
                match kind:
                    case Lit.Str:
                        self.buf += f"    mov {reg}, OFFSET FLAT:str{len(self.strings)}\n"
                        self.strings.append(value)
                    case _:
                        self.buf += f"    mov {reg}, {value}\n"
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
                    if node.alignment:
                        self.buf += f"    sub rsp, {node.alignment}\n"
                    for i, arg in enumerate(args):
                        off = self.scopes.def_local(arg.name, arg.ty)
                        self.buf += f"    mov [rbp - {off}], {fn_regs[i]}\n"
                    self.gen(body.stmts)
                    if node.alignment:
                        self.buf += f"    add rsp, {node.alignment}\n"
                    self.buf += "    pop rbp\n    ret\n"
                case Let(name, ty, init):
                    self.expr(init, "rax")
                    off = self.scopes.def_local(name, ty)
                    self.buf += f"    mov [rbp - {off}], rax\n"
                case Binary() | Call():
                    self.expr(node, "rax")
                case _:
                    assert False, f"{type(node)} is not implemented"
        self.scopes.pop()

    def emit_data(self):
        self.buf += ".data\n"
        for i, string in enumerate(self.strings):
            self.buf += f"str{i}:\n"
            self.buf += f"    .string {string}\n"

    def emit(self):
        self.buf += ".intel_syntax noprefix\n.text\n.globl main\n"
        self.gen(self.ast)
        self.emit_data()
        return self.buf


def main(argv):
    if len(argv) < 2:
        print("Usage: ./ray filename")
        exit(1)

    filename = argv[1]
    lexer = lexer_from_file(filename)
    src = lexer.program
    tokens = list(lexer.lexfile())
    parser = Parser(src, tokens)
    ast = list(parser.parse())
    tychk = TyCheck(ast)
    # gen = IRGen(ast)
    # pp(ast, max_depth=10)
    code = Codegen(ast, tychk.defs).emit()
    with open("test.asm", "w") as f:
        f.write(code)

    from subprocess import call
    call(['as', 'test.asm', '-o', 'test.o'])
    call(['gcc', 'test.o', '-o', 'ray'])


if __name__ == "__main__":
    main(sys.argv)
