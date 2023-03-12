from __future__ import annotations
from typing import List, Tuple, Dict
from ..Ast import Ty
from .inst import Label, Value, IConst
from .basic_block import BasicBlock


class FnDef:
    __match_args__ = ('name', 'args', 'ret_ty', 'basic_blocks')

    def __init__(self,
                 name: str,
                 args: List[Value],
                 ret_ty: Ty | None,
                 basic_blocks: List[BasicBlock]) -> None:
        self.name = name
        self.args = args
        self.ret_ty = ret_ty
        self.basic_blocks = basic_blocks

    @property
    def edges(self) -> List[Tuple[BasicBlock, BasicBlock]]:
        edges_ = []
        for a in self.basic_blocks:
            for b in a.succ:
                edges_.append((a, b, ))
        return edges_

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


class IRModule:
    def __init__(self) -> None:
        self.defs: List[FnDef] = []
        self.decls: List[FnDecl] = []
        self.consts: Dict[str, Tuple[Label, IConst]] = {}
        self.anon_consts: Dict[Label, IConst] = {}

    def __repr__(self) -> str:
        decls = "\n".join(map(str, self.decls))
        consts = "\n"
        for name, const in self.consts.items():
            consts += "const {} = {} ;; {}\n".format(const[0], const[1], name)
        for name, const in self.anon_consts.items():
            consts += "const {} = {}\n".format(name, const)
        defs = "\n".join(map(str, self.defs))
        return "\n{}\n{}\n{}".format(decls, consts, defs)
