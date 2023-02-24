from __future__ import annotations
from typing import List
from .inst import *


class BasicBlock:
    _id = 0

    def __init__(self,
                 instructions: List[Inst],
                 succ: List[BasicBlock],
                 pred: List[BasicBlock]) -> None:
        self.succ = succ
        self.pred = pred
        self.instructions = instructions
        self.processed = False
        self.bb_id = BasicBlock._id
        BasicBlock._id += 1

    def is_root(self) -> bool:
        return self.bb_id == 0

    def add(self, inst: Inst):
        self.instructions.append(inst)

    def __str__(self) -> str:
        return "bb{}:\n{}".format(self.bb_id, "\n".join(map(str, self.instructions)))
