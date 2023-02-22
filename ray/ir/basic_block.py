from __future__ import annotations
from typing import List
from .inst import *


class BasicBlock:
    _id = 0

    def __init__(self, instructions: List[Inst], parent: BasicBlock | None = None) -> None:
        self.parent = parent
        self.instructions = instructions
        self.bb_id = BasicBlock._id
        BasicBlock._id += 1

    def is_root(self) -> bool:
        return self.parent == None

    @property
    def len(self) -> InstId:
        if self.parent:
            return InstId(self.parent.len.i + len(self.instructions))
        return InstId(len(self.instructions))

    def add_inst(self, inst: Inst) -> InstId:
        inst_id = self.len
        self.instructions.append(inst)
        return inst_id

    def __str__(self) -> str:
        return "bb{}:\n{}".format(self.bb_id, "\n".join(map(str, self.instructions)))
