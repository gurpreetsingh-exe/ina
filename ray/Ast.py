from __future__ import annotations
from typing import List
from enum import Enum, auto
from .utils import *
from .Token import TokenKind, Span


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
    def get_size(self) -> int:
        raise NotImplemented()


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
            case _: assert False, f"get_size({self})"

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


class PtrTy(Ty):
    __match_args__ = ("ptr", )

    def __init__(self, ptr: Ty):
        self.ptr = ptr
        self.span = None

    def get_size(self) -> int:
        return 8

    def is_int(self) -> bool:
        return self.ptr.is_int()

    def is_float(self) -> bool:
        return self.ptr.is_float()

    def __eq__(self, __o: Ty) -> bool:
        match __o:
            case PtrTy(ptr):
                return ptr == self.ptr
            case _:
                return False

    def __ne__(self, __o: Ty) -> bool:
        return not self.__eq__(__o)

    def __repr__(self) -> str:
        return f"*{repr(self.ptr)}"


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


class StructTy(Ty):
    __match_args__ = ("name", "fields", )

    def __init__(self, name, fields: List[Ty]):
        self.name = name
        self.fields = fields
        self.span = None

    def get_size(self) -> int:
        sz = 0
        for field in self.fields:
            sz += ((field.get_size() + 3) // 4) * 4
        return sz

    def is_int(self) -> bool:
        return False

    def is_float(self) -> bool:
        return False

    def __eq__(self, __o: Ty) -> bool:
        match __o:
            case StructTy(fields):
                return __o.name == self.name and all([a == b for a, b in zip(fields, self.fields)])
            case _:
                return False

    def __ne__(self, __o: Ty) -> bool:
        return not self.__eq__(__o)

    def __repr__(self) -> str:
        f = ", ".join(map(repr, self.fields))
        return f"{self.name} {{ {f} }}"


class FnTy(Ty):
    __match_args__ = ("args", "ret_ty", )

    def __init__(self, args: List[Ty], ret_ty: Ty):
        self.args = args
        self.ret_ty = ret_ty
        self.span = None

    @classmethod
    def dummy(cls):
        return cls([], PrimTy(PrimTyKind.Unit))

    def get_size(self) -> int:
        return 8

    def __eq__(self, __o: Ty) -> bool:
        match __o:
            case FnTy(args, ret_ty):
                return all([arg == other for arg, other in zip(self.args, args)]) and self.ret_ty == ret_ty
            case _:
                return False

    def __ne__(self, __o: Ty) -> bool:
        return not self.__eq__(__o)

    def __repr__(self) -> str:
        return "fn ({}) -> {}".format(", ".join(map(repr, self.args)), self.ret_ty)


class ArrayTy(Ty):
    __match_args__ = ("ty", "length", )

    def __init__(self, ty: Ty, length: int):
        self.ty = ty
        self.length = length
        self.span = None

    def get_size(self) -> int:
        return self.ty.get_size() * self.length

    def __eq__(self, __o: Ty) -> bool:
        match __o:
            case ArrayTy(ty, length):
                return ty == self.ty and length == self.length
            case _:
                return False

    def __ne__(self, __o: Ty) -> bool:
        return not self.__eq__(__o)

    def __repr__(self) -> str:
        return f"[{repr(self.ty)}; {self.length}]"


class FnArg:
    pass


class Arg(FnArg):
    __match_args__ = ("name", "ty", )

    def __init__(self, name, ty):
        self.name = name
        self.ty = ty
        self.span: Span | None = None


class VariadicTy(Ty):
    pass


class Variadic(FnArg):
    def __init__(self) -> None:
        self.ty = VariadicTy()


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
        self.span: Span | None = None

    @property
    def is_variadic(self) -> bool:
        for arg in self.args:
            match arg:
                case Variadic():
                    return True
        return False


class Block:
    __match_args__ = ("stmts", )

    def __init__(self, stmts: List[Stmt], expr: Expr | None = None):
        self.stmts = stmts
        self.expr = expr
        self.span: Span | None = None


class ExternBlock:
    __match_args__ = ('items', )

    def __init__(self, items: List[Fn]) -> None:
        self.items = items
        self.span = None


class BinaryKind(Enum):
    Add = auto()
    Sub = auto()
    Mul = auto()
    Div = auto()
    Lt = auto()
    Gt = auto()
    Mod = auto()
    Eq = auto()
    NotEq = auto()

    def to_display(self):
        match self:
            case BinaryKind.Add: return "+"
            case BinaryKind.Sub: return "-"
            case BinaryKind.Mul: return "*"
            case BinaryKind.Div: return "/"
            case BinaryKind.Gt: return ">"
            case BinaryKind.Lt: return "<"
            case BinaryKind.Mod: return "%"
            case BinaryKind.Eq: return "=="
            case BinaryKind.NotEq: return "!="


def binary_kind_from_token(kind: TokenKind) -> BinaryKind:
    match kind:
        case TokenKind.PLUS: return BinaryKind.Add
        case TokenKind.MINUS: return BinaryKind.Sub
        case TokenKind.STAR: return BinaryKind.Mul
        case TokenKind.SLASH: return BinaryKind.Div
        case TokenKind.GT: return BinaryKind.Gt
        case TokenKind.LT: return BinaryKind.Lt
        case TokenKind.PERCENT: return BinaryKind.Mod
        case TokenKind.EQ2: return BinaryKind.Eq
        case TokenKind.BANGEQ: return BinaryKind.NotEq
        case _: assert False


class Binary:
    __match_args__ = ("kind", "left", "right")

    def __init__(self, kind: BinaryKind, left: Expr, right: Expr):
        self.kind = kind
        self.left = left
        self.right = right
        self.ty: Ty | None = None
        self.span: Span | None = None


class UnaryKind(Enum):
    Neg = auto()
    Not = auto()
    AddrOf = auto()
    Deref = auto()

    def to_display(self) -> str:
        match self:
            case UnaryKind.Neg: return "-"
            case UnaryKind.Not: return "!"
            case UnaryKind.AddrOf: return "&"
            case UnaryKind.Deref: return "*"


def unary_kind_from_token(kind: TokenKind) -> UnaryKind:
    match kind:
        case TokenKind.MINUS: return UnaryKind.Neg
        case TokenKind.BANG: return UnaryKind.Not
        case TokenKind.AMPERSAND: return UnaryKind.AddrOf
        case TokenKind.STAR: return UnaryKind.Deref
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


class Return:
    __match_args__ = ('expr', )

    def __init__(self, expr: Expr) -> None:
        self.expr = expr
        self.span: Span | None = None


class Stmt:
    __match_args__ = ('kind', )

    def __init__(self, kind) -> None:
        self.kind = kind
        self.span: Span | None = None
        self.semi = True


class Let:
    __match_args__ = ("name", "ty", "init")

    def __init__(self, name, ty, init):
        self.name = name
        self.ty = ty
        self.init = init
        self.span: Span | None = None


class Const:
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
        self.ty: Ty | None = None


class Ident:
    __match_args__ = ("name", )

    def __init__(self, name):
        self.name = name
        self.ty: Ty | None = None
        self.span: Span | None = None


class Cast:
    __match_args__ = ("expr", "ty", )

    def __init__(self, expr, ty):
        self.expr = expr
        self.ty = ty
        self.span: Span | None = None


class ArrayNor:
    __match_args__ = ('items', )

    def __init__(self, items: List[Expr]) -> None:
        self.items = items


class ArrayRepeat:
    __match_args__ = ('item', 'length', )

    def __init__(self, item: Expr, length: int) -> None:
        self.item = item
        self.length = length


Array = ArrayNor | ArrayRepeat


class StructField:
    __match_args__ = ("name", "ty", )

    def __init__(self, name, ty) -> None:
        self.name = name
        self.ty = ty
        self.offset = 0
        self.span = None


class Struct:
    __match_args__ = ("name", "fields", )

    def __init__(self, name, fields: List[StructField]) -> None:
        self.name = name
        self.fields = fields
        self.span = None


class ExprField:
    __match_args__ = ("name", "expr", )

    def __init__(self, name, expr: Expr) -> None:
        self.name = name
        self.expr = expr
        self.span = None


class StructExpr:
    __match_args__ = ("name", "fields", )

    def __init__(self, name, fields: List[ExprField]) -> None:
        self.name = name
        self.fields = fields
        self.span = None


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
        self.ty: Ty | None = None
        self.span: Span | None = None


Item = Const | Fn | ExternBlock


class Module:
    __match_args__ = ("items", )

    def __init__(self, items: List[Item], name: str) -> None:
        self.items = items
        self.name = name
        self._itr_index = 0

    def __iter__(self) -> Module:
        self._itr_index = 0
        return self

    def __next__(self) -> Item:
        if self._itr_index < len(self.items):
            item = self.items[self._itr_index]
            self._itr_index += 1
            return item
        raise StopIteration()


Expr = Array \
    | Binary \
    | Unary \
    | Call \
    | Literal \
    | Ident \
    | Cast \
    | Assign \
    | Loop \
    | If
