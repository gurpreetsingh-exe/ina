from __future__ import annotations
from typing import List, Set
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
        self.dominators: List[BasicBlock] = []
        self.dominance_frontiers: List[BasicBlock] = []
        self.idom: BasicBlock
        self.live_ins: Set[Inst] = set()
        self.live_outs: Set[Inst] = set()
        BasicBlock._id += 1

    @staticmethod
    def reset():
        BasicBlock._id = 0

    def is_root(self) -> bool:
        return self.bb_id == 0

    def uses(self, i: Inst) -> bool:
        for inst in self.instructions:
            if inst.uses(i):
                return True
        return False

    def add(self, inst: Inst):
        self.instructions.append(inst)

    def dominates(self, block: BasicBlock) -> bool:
        return block in self.dominators

    def strictly_dominates(self, block: BasicBlock) -> bool:
        return block in self.dominators and self != block

    def __str__(self) -> str:
        return "\nbb{}:                             ; preds: {}\n{}".format(
            self.bb_id,
            ", ".join(map(lambda b: f"%bb{b.bb_id}", self.pred)),
            "\n".join(map(str, self.instructions)))

    def __repr__(self) -> str:
        return "\033[1;31mbb{}\033[0m".format(self.bb_id)

    @property
    def df(self) -> str:
        return "DF(bb{}): {{{}}}".format(self.bb_id, ", ".join(map(lambda b: f"bb{b.bb_id}", self.dominance_frontiers)))

    @property
    def dom(self) -> str:
        return "DOM(bb{}): {{{}}}".format(self.bb_id, ", ".join(map(lambda b: f"bb{b.bb_id}", self.dominators)))

    @property
    def imm_dom(self) -> str:
        return "IDOM(bb{}): bb{}".format(self.bb_id, self.idom.bb_id)