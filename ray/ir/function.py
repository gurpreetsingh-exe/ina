from __future__ import annotations
from typing import List
from ..Ast import Ty
from .inst import Value
from .basic_block import BasicBlock


class FnDef:
    def __init__(self,
                 name: str,
                 args: List[Value],
                 ret_ty: Ty | None,
                 basic_blocks: List[BasicBlock]) -> None:
        self.name = name
        self.args = args
        self.ret_ty = ret_ty
        self.basic_blocks = basic_blocks

    def __str__(self) -> str:
        return "def {}({}) -> {} {{\n{}\n}}".format(
            self.name,
            ", ".join(map(str, self.args)),
            self.ret_ty,
            "\n".join(map(str, self.basic_blocks)))


class FnDecl:
    def __init__(self,
                 name: str,
                 args: List[Value],
                 ret_ty: Ty | None) -> None:
        self.name = name
        self.args = args
        self.ret_ty = ret_ty

    def __str__(self) -> str:
        return "decl {}({}) -> {}".format(
            self.name,
            ", ".join(map(str, self.args)),
            self.ret_ty)
