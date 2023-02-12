from __future__ import annotations
from Ast import *
from typing import Dict


class Instruction:
    def __init__(self, kind, inst_id) -> None:
        self.kind = kind
        self.inst_id = inst_id

    def has_ret_ty(self) -> bool:
        match self.kind:
            case Store() | Jmp() | Br():
                return False
            case _:
                return True

    def __repr__(self) -> str:
        global inst_id
        _ = "    "
        if self.has_ret_ty():
            _ += f"%{self.inst_id} = "

        return _ + repr(self.kind)


class FnCall:
    __match_args__ = ('name', 'args', 'va_args', )

    def __init__(self, name: str, args: List[Value], va_args: int) -> None:
        self.name = name
        self.args = args
        self.va_args = va_args

    def __repr__(self) -> str:
        _ = f"call @{self.name}"
        _ += "(" + ", ".join(map(repr, self.args)) + ")"
        return _

    @property
    def float_args(self) -> int:
        n = 0
        for arg in self.args:
            match arg.kind:
                case ValueKind.Imm:
                    # TODO: add float literals
                    pass
                case _:
                    pass
        return n


class Alloc:
    __match_args__ = ('ty', 'offset', )

    def __init__(self, ty: Ty, offset: int) -> None:
        self.ty = ty
        self.offset = offset

    def __repr__(self) -> str:
        return f"alloc {self.ty}"


class Store:
    __match_args__ = ('dst', 'src', )

    def __init__(self, dst: Value, src: Value) -> None:
        self.dst = dst
        self.src = src

    def __repr__(self) -> str:
        return f"store {self.dst}, {self.src}"


class Load:
    __match_args__ = ('ptr', )

    def __init__(self, ptr: Value) -> None:
        self.ptr = ptr

    def __repr__(self) -> str:
        return f"load {self.ptr}"


class BinaryInst:
    def __init__(self, left: Value, right: Value, name: str) -> None:
        self.left = left
        self.right = right
        self.name = name

    def __repr__(self) -> str:
        return f"{self.name} {self.left}, {self.right}"


class Add(BinaryInst):
    def __init__(self, left: Value, right: Value) -> None:
        super().__init__(left, right, "add")


class Sub(BinaryInst):
    def __init__(self, left: Value, right: Value) -> None:
        super().__init__(left, right, "sub")


class Mod(BinaryInst):
    def __init__(self, left: Value, right: Value) -> None:
        super().__init__(left, right, "mod")


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


class Cmp:
    def __init__(self, left: Value, right: Value, kind: CmpKind) -> None:
        self.left = left
        self.right = right
        self.kind = kind

    def __repr__(self) -> str:
        return f"cmp {repr(self.kind)} {self.left}, {self.right}"


class Br:
    def __init__(self, cond: Value, btrue: int, bfalse: int) -> None:
        self.cond = cond
        self.btrue = btrue
        self.bfalse = bfalse

    def __repr__(self) -> str:
        return f"br {self.cond}, %bb{self.btrue}, %bb{self.bfalse}"


class Jmp:
    def __init__(self, br_id: int) -> None:
        self.br_id = br_id

    def __repr__(self) -> str:
        return f"jmp %bb{self.br_id}"


class Phi:
    def __init__(self, true_br: int, false_br: int) -> None:
        self.true_br = true_br
        self.false_br = false_br

    def __repr__(self) -> str:
        return f"phi [%bb{self.true_br}, %bb{self.false_br}]"


class BasicBlock:
    def __init__(self, instructions: List[Instruction], parent: int | None, block_id) -> None:
        self.instructions = instructions
        self.parent = parent
        self.locals: Dict[str, Value] = {}
        self.block_id = block_id

    def __repr__(self) -> str:
        if self.instructions:
            return f"bb{self.block_id}:\n" + "\n".join(map(repr, self.instructions)) + "\n"
        else:
            return f"bb{self.block_id}:\n"

    def get_var(self, name: str) -> Value | None:
        if name in self.locals:
            return self.locals[name]


class ValueKind(Enum):
    SymbolId = auto()
    InstId = auto()
    Imm = auto()


class Value:
    def __init__(self, kind: ValueKind, data) -> None:
        self.kind = kind
        self.data = data

    def __repr__(self) -> str:
        match self.kind:
            case ValueKind.SymbolId:
                return f"@{self.data}"
            case ValueKind.InstId:
                return f"%{self.data}"
            case ValueKind.Imm:
                return f"{self.data}"


class Param:
    def __init__(self, name: str = "", ty: Ty | None = None) -> None:
        self.name = name
        self.ty = ty
        self.variadic = False

    def __repr__(self) -> str:
        if self.variadic:
            return "..."
        return f"%{self.name}: {self.ty}"


class FnDef:
    __match_args__ = ('name', 'params', 'ret_ty', 'blocks', )

    def __init__(self,
                 name: str,
                 params: List[Value],
                 ret_ty: Ty | None,
                 blocks: List[BasicBlock]) -> None:
        self.name = name
        self.params = params
        self.ret_ty = ret_ty
        self.blocks = blocks

    @property
    def stack_alignment(self) -> int:
        off = 0
        for block in self.blocks:
            for inst in block.instructions:
                match inst.kind:
                    case Alloc(ty):
                        off += ty.get_size()
        return (off + 15) & ~15

    def __repr__(self) -> str:
        _ = f"def @{self.name}"
        _ += "(" + ", ".join(map(repr, self.params)) + ")"
        if self.ret_ty:
            _ += f" -> {self.ret_ty} "
        _ += "{\n" + "".join(map(repr, self.blocks)) + "}"
        return _


class FnDecl:
    __match_args__ = ('name', 'params', 'ret_ty', )

    def __init__(self,
                 name: str,
                 params: List[Value],
                 ret_ty: Ty | None) -> None:
        self.name = name
        self.params = params
        self.ret_ty = ret_ty

    def __repr__(self) -> str:
        _ = f"decl @{self.name}"
        _ += "(" + ", ".join(map(repr, self.params)) + ")"
        if self.ret_ty:
            _ += f" -> {self.ret_ty}"
        return _
