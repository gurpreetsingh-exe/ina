from __future__ import annotations
from typing import List
from ..Ast import Literal, Ty, BinaryKind, Lit
from enum import Enum, auto
from .basic_block import BasicBlock


class Value:
    pass


class Inst(Value):
    _id = 0

    def __init__(self, name=None) -> None:
        if not name:
            self.inst_id = Inst._id
            Inst._id += 1
        self.name = name
        self.parent: BasicBlock
        self.ty: Ty

    @classmethod
    def reset(cls):
        cls._id = 0

    def uses(self, i: Inst) -> bool:
        raise NotImplemented()

    def __str__(self) -> str:
        return "%{}".format(self.inst_id if not self.name else self.name)

    def __repr__(self) -> str:
        return "%{}".format(self.inst_id if not self.name else self.name)


class Alloc(Inst):
    def __init__(self, ty: Ty, off: int, name: str) -> None:
        super().__init__()
        self.ty = ty
        self.size = ty.get_size()
        self.off = off
        self.var_name = name
        self.userz: List[Inst] = []

    def users(self, basic_block: BasicBlock) -> List[Inst]:
        users = set()
        for inst in basic_block.instructions:
            if inst.uses(self):
                users.add(inst)
        return list(users)

    def remove_from_parent(self):
        self.parent.instructions.remove(self)

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

    def uses(self, i: Inst) -> bool:
        match self.src:
            case IConst():
                return self.dst == i
            case _:
                return self.dst == i or self.src == i

    def __str__(self) -> str:
        return "    store {}, {}".format(repr(self.dst), repr(self.src))


class Load(Inst):
    def __init__(self, src: Inst) -> None:
        super().__init__()
        self.src = src

    def uses(self, i: Inst) -> bool:
        return self.src == i

    def __str__(self) -> str:
        return "    {} = load {}".format(super().__str__(), repr(self.src))


class CmpKind(Enum):
    Lt = auto()
    Gt = auto()
    Eq = auto()
    NotEq = auto()

    @staticmethod
    def from_binary(kind: BinaryKind):
        match kind:
            case BinaryKind.Lt:
                return CmpKind.Lt
            case BinaryKind.Gt:
                return CmpKind.Gt
            case BinaryKind.Eq:
                return CmpKind.Eq
            case BinaryKind.NotEq:
                return CmpKind.NotEq
            case _:
                assert False, kind

    def __repr__(self) -> str:
        match self:
            case CmpKind.Lt:
                return "lt"
            case CmpKind.Gt:
                return "gt"
            case CmpKind.Eq:
                return "eq"
            case CmpKind.NotEq:
                return "noteq"


class Cmp(Inst):
    def __init__(self, left: Value, right: Value, kind: CmpKind) -> None:
        super().__init__()
        self.left = left
        self.right = right
        self.kind = kind

    def uses(self, i: Inst) -> bool:
        return self.left == i or self.right == i

    def __str__(self) -> str:
        return "    {} = cmp {} {}, {}".format(super().__str__(), repr(self.kind), repr(self.left), repr(self.right))


class Bin(Inst):
    def __init__(self, left: Value, right: Value) -> None:
        super().__init__()
        self.left = left
        self.right = right

    def uses(self, i: Inst) -> bool:
        return self.left == i or self.right == i

    def __str__(self) -> str:
        return "    {} = {} {}, {}".format(super().__str__(), self.inst_name, repr(self.left), repr(self.right))


class Add(Bin):
    def __init__(self, left: Value, right: Value) -> None:
        super().__init__(left, right)
        self.inst_name = "add"


class Sub(Bin):
    def __init__(self, left: Value, right: Value) -> None:
        super().__init__(left, right)
        self.inst_name = "sub"


class Mul(Bin):
    def __init__(self, left: Value, right: Value) -> None:
        super().__init__(left, right)
        self.inst_name = "mul"


class Div(Bin):
    def __init__(self, left: Value, right: Value) -> None:
        super().__init__(left, right)
        self.inst_name = "div"


class Mod(Bin):
    def __init__(self, left: Value, right: Value) -> None:
        super().__init__(left, right)
        self.inst_name = "mod"


class Br(Inst):
    __match_args__ = ('cond', 'btrue', 'bfalse', )

    def __init__(self, cond: Value, btrue: int, bfalse: int) -> None:
        super().__init__()
        self.cond = cond
        self.btrue = btrue
        self.bfalse = bfalse

    def uses(self, i: Inst):
        return self.cond == i

    def __str__(self) -> str:
        return "    br {}, %bb{}, %bb{}".format(repr(self.cond), self.btrue, self.bfalse)


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


class IConst(Value):
    def __init__(self, kind, value) -> None:
        self.kind = kind
        self.value = value

    @staticmethod
    def from_lit(lit: Literal) -> IConst:
        match lit.kind:
            case Lit.Int:
                return IConst(ConstKind.Int, lit.value)
            case Lit.Float:
                return IConst(ConstKind.Float, lit.value)
            case Lit.Str:
                return IConst(ConstKind.Str, lit.value)
            case Lit.Bool:
                return IConst(ConstKind.Bool, lit.value)
            case _:
                assert False, lit.kind

    def __str__(self) -> str:
        return self.value

    __repr__ = __str__
