from __future__ import annotations
from typing import List
from enum import Enum, auto
from utils import *
from Token import TokenKind, Span


def get_ty(name) -> PrimTyKind | None:
    match name:
        case "u8": return PrimTyKind.U8
        case "u16": return PrimTyKind.U16
        case "u32": return PrimTyKind.U32
        case "u64": return PrimTyKind.U64
        case "usize": return PrimTyKind.USize
        case "i8": return PrimTyKind.I8
        case "i16": return PrimTyKind.I16
        case "i32": return PrimTyKind.I32
        case "i64": return PrimTyKind.I64
        case "isize": return PrimTyKind.ISize
        case "f32": return PrimTyKind.F32
        case "f64": return PrimTyKind.F64
        case "bool": return PrimTyKind.Bool
        case "char": return PrimTyKind.Char
        case "str": return PrimTyKind.Str
        case "raw": return PrimTyKind.Raw
        case "()": return PrimTyKind.Unit


class Ty:
    pass


class PrimTyKind(Enum):
    U8 = auto()
    U16 = auto()
    U32 = auto()
    U64 = auto()
    USize = auto()
    I8 = auto()
    I16 = auto()
    I32 = auto()
    I64 = auto()
    ISize = auto()
    F32 = auto()
    F64 = auto()
    Bool = auto()
    Char = auto()
    Str = auto()
    Raw = auto()
    Unit = auto()

    def get_size(self) -> int:
        match self:
            case PrimTyKind.U8: return 1
            case PrimTyKind.U16: return 2
            case PrimTyKind.U32: return 4
            case PrimTyKind.U64: return 8
            case PrimTyKind.USize: return 8
            case PrimTyKind.I8: return 1
            case PrimTyKind.I16: return 2
            case PrimTyKind.I32: return 4
            case PrimTyKind.I64: return 8
            case PrimTyKind.ISize: return 8
            case PrimTyKind.F32: return 4
            case PrimTyKind.F64: return 8
            case PrimTyKind.Bool: return 1
            case PrimTyKind.Char: return 1
            case PrimTyKind.Str: return 8
            case PrimTyKind.Raw: return 8
            case PrimTyKind.Unit: return 0

    def is_number(self) -> bool:
        match self:
            case PrimTyKind.Bool \
                | PrimTyKind.Char \
                | PrimTyKind.Str \
                | PrimTyKind.Unit \
                | PrimTyKind.Raw: return False
            case _: return True

    def is_float(self) -> bool:
        match self:
            case PrimTyKind.F32 | PrimTyKind.F64: return True
        return False

    def is_int(self) -> bool:
        match self:
            case PrimTyKind.Bool \
                | PrimTyKind.Char \
                | PrimTyKind.Str \
                | PrimTyKind.Unit \
                | PrimTyKind.F32 \
                | PrimTyKind.F64 \
                | PrimTyKind.Raw: return False
        return True

    def __repr__(self) -> str:
        match self:
            case PrimTyKind.U8: return "u8"
            case PrimTyKind.U16: return "u16"
            case PrimTyKind.U32: return "u32"
            case PrimTyKind.U64: return "u64"
            case PrimTyKind.USize: return "usize"
            case PrimTyKind.I8: return "i8"
            case PrimTyKind.I16: return "i16"
            case PrimTyKind.I32: return "i32"
            case PrimTyKind.I64: return "i64"
            case PrimTyKind.ISize: return "isize"
            case PrimTyKind.F32: return "f32"
            case PrimTyKind.F64: return "f64"
            case PrimTyKind.Bool: return "bool"
            case PrimTyKind.Char: return "char"
            case PrimTyKind.Str: return "str"
            case PrimTyKind.Raw: return "raw"
            case PrimTyKind.Unit: return "()"


class PrimTy(Ty):
    __match_args__ = ("kind", )

    def __init__(self, kind: PrimTyKind):
        self.kind = kind
        self.span: Span | None = None

    def is_int(self) -> bool:
        return self.kind.is_int()

    def is_float(self) -> bool:
        return self.kind.is_float()

    def get_size(self) -> int:
        return self.kind.get_size()

    def __eq__(self, __o: Ty) -> bool:
        match __o:
            case PrimTy(kind):
                return kind == self.kind
            case _:
                return False

    def __ne__(self, __o: Ty) -> bool:
        return not self.__eq__(__o)

    def __repr__(self) -> str:
        return repr(self.kind)


class RefTy(Ty):
    __match_args__ = ("ref", )

    def __init__(self, ref: Ty):
        self.ref = ref
        self.span = None

    def get_size(self) -> int:
        return 8

    def __eq__(self, __o: Ty) -> bool:
        match __o:
            case RefTy(ref):
                return ref == self.ref
            case _:
                return False

    def __ne__(self, __o: Ty) -> bool:
        return not self.__eq__(__o)

    def __repr__(self) -> str:
        return f"&{repr(self.ref)}"


class FnArg:
    pass


class Arg(FnArg):
    __match_args__ = ("name", "ty", )

    def __init__(self, name, ty):
        self.name = name
        self.ty = ty
        self.span: Span | None = None


class Variadic(FnArg):
    pass


class Fn:
    __match_args__ = ("name", "args", "ret_ty", "body", "is_extern", "abi", )

    def __init__(
        self,
        name: str,
        args: List[FnArg],
        ret_ty: Ty | None,
        body: Block | None,
        is_extern: bool,
        abi: str | None
    ):
        self.name = name
        self.args = args
        self.ret_ty = ret_ty
        self.body = body
        self.is_extern = is_extern
        self.abi = abi
        self.alignment = ((self.local_count + 1) // 2) * 16
        self.span: Span | None = None

    def aligned_add(self, off, size):
        off += size
        return off

    @property
    def stack_offset(self):
        stack_off = 0
        for arg in self.args:
            match arg:
                case Arg(_, ty):
                    size = ty.get_size()
                    stack_off = self.aligned_add(stack_off, size)
        if not self.body:
            return stack_off

        stack_off += self.body.calc_stack()
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
    __match_args__ = ("stmts", )

    def __init__(self, stmts: List[Stmt]):
        self.stmts = stmts
        self.span: Span | None = None

    def calc_stack(self) -> int:
        off = 0
        for stmt in self.stmts:
            match stmt.kind:
                case Let(_, ty, _):
                    size = ty.get_size()
                    off += size
                case Expr(kind):
                    match kind:
                        case Loop(block):
                            off += block.calc_stack()
                        case If(_, if_block, else_block):
                            off += if_block.calc_stack()
                            if else_block:
                                off += else_block.calc_stack()
        return off


class Expr:
    __match_args__ = ("kind", )

    def __init__(self, kind) -> None:
        self.kind = kind
        self.ty: Ty | None = None
        self.span: Span | None = None


class BinaryKind(Enum):
    Add = auto()
    Sub = auto()
    Mul = auto()
    Div = auto()
    Lt = auto()
    Gt = auto()
    Mod = auto()


def binary_kind_from_token(kind: TokenKind) -> BinaryKind:
    match kind:
        case TokenKind.PLUS: return BinaryKind.Add
        case TokenKind.MINUS: return BinaryKind.Sub
        case TokenKind.STAR: return BinaryKind.Mul
        case TokenKind.SLASH: return BinaryKind.Div
        case TokenKind.GT: return BinaryKind.Gt
        case TokenKind.LT: return BinaryKind.Lt
        case TokenKind.PERCENT: return BinaryKind.Mod
        case _: assert False


class Binary:
    __match_args__ = ("kind", "left", "right")

    def __init__(self, kind: BinaryKind, left: Expr, right: Expr):
        self.kind = kind
        self.left = left
        self.right = right
        self.ty = None
        self.span: Span | None = None


class UnaryKind(Enum):
    Neg = auto()
    Not = auto()
    AddrOf = auto()


def unary_kind_from_token(kind: TokenKind) -> UnaryKind:
    match kind:
        case TokenKind.MINUS: return UnaryKind.Neg
        case TokenKind.BANG: return UnaryKind.Not
        case TokenKind.AMPERSAND: return UnaryKind.AddrOf
        case _: assert False


class Unary:
    __match_args__ = ("kind", "expr", )

    def __init__(self, kind: UnaryKind, expr: Expr):
        self.kind = kind
        self.expr = expr
        self.ty = None
        self.span: Span | None = None


class Call:
    __match_args__ = ("name", "args", )

    def __init__(self, name, args: List[Expr]):
        self.name = name
        self.args = args
        self.ty = None
        self.span: Span | None = None


class If:
    __match_args__ = ("cond", "body", "elze", )

    def __init__(self, cond, body, elze=None):
        self.cond = cond
        self.body = body
        self.elze = elze
        self.ty = None
        self.span: Span | None = None


class Else:
    __match_args__ = ("body", )

    def __init__(self, body):
        self.body = body
        self.span: Span | None = None


class Loop:
    __match_args__ = ("body", )

    def __init__(self, body: Block) -> None:
        self.body = body
        self.span: Span | None = None


class Break:
    def __init__(self) -> None:
        self.label = 0


class Stmt:
    def __init__(self, kind) -> None:
        self.kind = kind
        self.span: Span | None = None


class Let:
    __match_args__ = ("name", "ty", "init")

    def __init__(self, name, ty, init):
        self.name = name
        self.ty = ty
        self.init = init
        self.span: Span | None = None


class Assign:
    __match_args__ = ("name", "init", )

    def __init__(self, name, init):
        self.name = name
        self.init = init
        self.span: Span | None = None


class Ident:
    __match_args__ = ("name", )

    def __init__(self, name):
        self.name = name
        self.ty = None
        self.span: Span | None = None


class Cast:
    __match_args__ = ("expr", "ty", )

    def __init__(self, expr, ty):
        self.expr = expr
        self.ty = ty
        self.span: Span | None = None


class Lit(Enum):
    Int = auto()
    Float = auto()
    Bool = auto()
    Str = auto()
    Char = auto()


class Literal:
    __match_args__ = ("kind", "value", )

    def __init__(self, kind, value):
        self.kind = kind
        self.value = value
        self.ty = None
        self.span: Span | None = None
