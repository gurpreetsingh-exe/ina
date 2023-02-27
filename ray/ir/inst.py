from __future__ import annotations
from ..Ast import Literal, Ty, BinaryKind, Lit
from enum import Enum, auto
from .basic_block import BasicBlock


class Value:
    pass


class InstId(Value):
    def __init__(self, i: int) -> None:
        self._i = i

    @property
    def i(self) -> int:
        return self._i

    def __repr__(self) -> str:
        return f"%{self._i}"


class Inst(Value):
    _id = 0

    def __init__(self, name=None) -> None:
        if not name:
            self.inst_id = Inst._id
            Inst._id += 1
        self.name = name
        self.parent: BasicBlock

    @classmethod
    def reset(cls):
        cls._id = 0

    def uses(self, i: Inst) -> bool:
        raise NotImplemented()

    def __str__(self) -> str:
        return "%{}".format(self.inst_id if not self.name else self.name)


class Alloc(Inst):
    def __init__(self, ty: Ty, off: int, name: str) -> None:
        super().__init__()
        self.ty = ty
        self.off = off
        self.var_name = name

    def uses(self, _: Inst) -> bool:
        return False

    def __str__(self) -> str:
        return "    {} = alloc {}".format(super().__str__(), self.ty)


class Store(Inst):
    def __init__(self, dst: Value, src: Value, name: str) -> None:
        super().__init__()
        self.dst = dst
        self.src = src
        self.var_name = name

    def uses(self, _: Inst) -> bool:
        return False

    def __str__(self) -> str:
        return "    store {}, {}".format(self.dst, self.src)


class Load(Inst):
    def __init__(self, src: InstId) -> None:
        super().__init__()
        self.src = src

    def uses(self, i: Inst) -> bool:
        return self.src.i == i.inst_id

    def __str__(self) -> str:
        return "    {} = load {}".format(super().__str__(), self.src)


class CmpKind(Enum):
    Lt = auto()
    Gt = auto()

    @staticmethod
    def from_binary(kind: BinaryKind):
        match kind:
            case BinaryKind.Lt:
                return CmpKind.Lt
            case BinaryKind.Gt:
                return CmpKind.Gt
            case _:
                assert False

    def __repr__(self) -> str:
        match self:
            case CmpKind.Lt:
                return "lt"
            case CmpKind.Gt:
                return "gt"


class Cmp(Inst):
    def __init__(self, left: Value, right: Value, kind: CmpKind) -> None:
        super().__init__()
        self.left = left
        self.right = right
        self.kind = kind

    def uses(self, i: Inst) -> bool:
        return self.left.i == i.inst_id or self.right.i == i.inst_id

    def __str__(self) -> str:
        return "    {} = cmp {} {}, {}".format(super().__str__(), repr(self.kind), self.left, self.right)


class Br(Inst):
    __match_args__ = ('cond', 'btrue', 'bfalse', )

    def __init__(self, cond: Value, btrue: int, bfalse: int) -> None:
        super().__init__()
        self.cond = cond
        self.btrue = btrue
        self.bfalse = bfalse

    def uses(self, i: Inst):
        return self.cond.i == i.inst_id

    def __str__(self) -> str:
        return "    br {}, %bb{}, %bb{}".format(self.cond, self.btrue, self.bfalse)


class Jmp(Inst):
    __match_args__ = ('br_id', )

    def __init__(self, br_id: int) -> None:
        super().__init__()
        self.br_id = br_id

    def uses(self, _: Inst) -> bool:
        return False

    def __str__(self) -> str:
        return "    jmp %bb{}".format(self.br_id)


class Phi(Inst):
    def __init__(self, btrue: Value, bfalse: Value) -> None:
        super().__init__()
        self.btrue = btrue
        self.bfalse = bfalse

    def uses(self, _: Inst) -> bool:
        return False

    def __str__(self) -> str:
        return "    {} = phi {}, {}".format(super().__str__(), self.btrue, self.bfalse)


class Nop(Inst):
    def __init__(self) -> None:
        super().__init__()

    def uses(self, _: Inst) -> bool:
        return False

    def __str__(self) -> str:
        return "    nop"


class ConstKind(Enum):
    Int = auto()
    Float = auto()
    Str = auto()
    Bool = auto()


class Const(Value):
    def __init__(self, kind, value) -> None:
        self.kind = kind
        self.value = value

    @staticmethod
    def from_lit(lit: Literal) -> Const:
        match lit.kind:
            case Lit.Int:
                return Const(ConstKind.Int, lit.value)
            case Lit.Float:
                return Const(ConstKind.Float, lit.value)
            case Lit.Str:
                return Const(ConstKind.Str, lit.value)
            case Lit.Bool:
                return Const(ConstKind.Bool, lit.value)
            case _:
                assert False, lit.kind

    def __str__(self) -> str:
        return self.value
