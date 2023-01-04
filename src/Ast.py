from __future__ import annotations
from typing import List
from enum import Enum, auto
from utils import *

class TyKind(Enum):
    Int   = auto()
    Float = auto()
    Bool  = auto()
    Str   = auto()
    Char  = auto()
    Raw   = auto()

def get_prim_ty(ty):
    match ty:
        case 'int': return TyKind.Int
        case 'float': return TyKind.Float
        case 'bool': return TyKind.Bool
        case 'str': return TyKind.Str
        case 'char': return TyKind.Char
        case 'raw': return TyKind.Raw
        case _: panic("unexpected type")

class Ty:
    __match_args__ = ("ty", )
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

    def __eq__(self, __o: Ty) -> bool:
        return self.ty == __o.ty

    def __ne__(self, __o: Ty) -> bool:
        return not self.__eq__(__o)

    def __repr__(self):
        return str(self.ty)

class FnArg:
    pass

class Arg(FnArg):
    __match_args__ = ("name", "ty", )
    def __init__(self, name, ty):
        self.name = name
        self.ty = ty

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
            if type(node.kind) == Let:
                stack_off += node.kind.ty.get_size()
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

class Expr:
    __match_args__ = ("kind", )
    def __init__(self, kind) -> None:
        self.kind = kind
        self.ty: Ty | None = None

class Binary:
    __match_args__ = ("op", "left", "right")
    def __init__(self, op, left: Expr, right: Expr):
        self.op = op
        self.left = left
        self.right = right
        self.ty = None

class Unary:
    __match_args__ = ("op", "expr", )
    def __init__(self, op, expr: Expr):
        self.op = op
        self.expr = expr
        self.ty = None

class Call:
    __match_args__ = ("name", "args", )
    def __init__(self, name, args: List[Expr]):
        self.name = name
        self.args = args
        self.ty = None

class If:
    __match_args__ = ("cond", "body", "elze", )
    def __init__(self, cond, body, elze = None):
        self.cond = cond
        self.body = body
        self.elze = elze
        self.ty = None

class Else:
    __match_args__ = ("body", )
    def __init__(self, body):
        self.body = body

class Stmt:
    def __init__(self, kind) -> None:
        self.kind = kind

class Let:
    __match_args__ = ("name", "ty", "init")
    def __init__(self, name, ty, init):
        self.name = name
        self.ty = ty
        self.init = init

class Assign:
    __match_args__ = ("name", "init", )
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
    __match_args__ = ("kind", "value", )
    def __init__(self, kind, value):
        self.kind = kind
        self.value = value
        self.ty = None

