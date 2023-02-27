from typing import Set
from ..ir.basic_block import BasicBlock, Inst
from ..ir.inst import Phi
from .dominance_frontier import dominance_frontier
from .IrGen import LoweringContext


def insert_phi(ctx: LoweringContext):
    for v, blocks in ctx.defs.items():
        W: Set[BasicBlock] = set(blocks)
        F: Set[BasicBlock] = set()
        while W:
            X = W.pop()
            for Y in X.dominance_frontiers:
                if Y not in F:
                    Y.instructions.reverse()
                    Y.add(Phi(Inst(v), Inst(v)))
                    Y.instructions.reverse()
                    F.add(Y)
                    if v not in Y.defs:
                        W.add(Y)
