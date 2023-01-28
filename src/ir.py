from __future__ import annotations
from Ast import *

inst_id = 0


class Instruction:
    def __init__(self, kind) -> None:
        self.kind = kind

    def has_ret_ty(self) -> bool:
        match self.kind:
            case FnCall() | Alloc():
                return True
            case _:
                return False

    def __repr__(self) -> str:
        global inst_id
        _ = "    "
        if self.has_ret_ty():
            _ += f"%{inst_id} = "
            inst_id += 1

        return _ + repr(self.kind)


class FnCall:
    def __init__(self, name: str, args: List[Value]) -> None:
        self.name = name
        self.args = args

    def __repr__(self) -> str:
        _ = f"call @{self.name}"
        _ += "(" + ", ".join(map(repr, self.args)) + ")"
        return _


class Alloc:
    def __init__(self, ty: Ty) -> None:
        self.ty = ty

    def __repr__(self) -> str:
        return f"alloc {self.ty}"


class Store:
    def __init__(self, dst: Value, src: Value) -> None:
        self.dst = dst
        self.src = src

    def __repr__(self) -> str:
        return f"store {self.dst}, {self.src}"


class BasicBlock:
    def __init__(self, instructions: List[Instruction], parent: int | None) -> None:
        self.instructions = instructions
        self.parent = parent

    def __repr__(self) -> str:
        return "\n".join(map(repr, self.instructions))


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

        return repr(self.kind)


class FnDef:
    def __init__(self,
                 name: str,
                 params: List[Value],
                 ret_ty: Ty | None,
                 blocks: List[BasicBlock]) -> None:
        self.name = name
        self.params = params
        self.ret_ty = ret_ty
        self.blocks = blocks

    def __repr__(self) -> str:
        _ = f"def @{self.name}"
        _ += ", ".join(map(repr, self.params))
        if self.ret_ty:
            _ += f" -> {self.ret_ty} "
        _ += "{\n" + "\n".join(map(repr, self.blocks)) + "\n}"
        return _


class FnDecl:
    def __init__(self,
                 name: str,
                 params: List[Value],
                 ret_ty: Ty | None) -> None:
        self.name = name
        self.params = params
        self.ret_ty = ret_ty

    def __repr__(self) -> str:
        _ = f"def @{self.name}"
        _ += ", ".join(map(repr, self.params))
        if self.ret_ty:
            _ += f" -> {self.ret_ty}"
        return _
