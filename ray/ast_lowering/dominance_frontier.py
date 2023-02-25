"""
Dominance Frontier Algorithm

References
    [1] A Simple, Fast Dominance Algorithm
        Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy
"""

from functools import reduce
from typing import Dict, List
from ..ir.function import *
from ..ir.basic_block import *


def postorder(basic_block: BasicBlock) -> List[BasicBlock]:
    visited = set()
    order = []

    def dfs_walk(node: BasicBlock):
        visited.add(node)
        for succ in node.succ:
            if not succ in visited:
                dfs_walk(succ)
        order.append(node)
    dfs_walk(basic_block)
    return order


IDomMap = Dict[BasicBlock, BasicBlock]


def calc_idom(block: BasicBlock, basic_blocks: List[BasicBlock]) -> IDomMap:
    """Calculates Immediate Dominators of the nodes"""

    start = basic_blocks[0]
    idom: IDomMap = {start: start}

    def intersect(u: BasicBlock, v: BasicBlock):
        while u != v:
            while dfn[u] < dfn[v]:
                u = idom[u]
            while dfn[u] > dfn[v]:
                v = idom[v]
        return u

    order = postorder(block)
    dfn = {u: i for i, u in enumerate(order)}
    if start in order:
        order.remove(start)
    order.reverse()

    changed = True
    while changed:
        changed = False
        for u in order:
            new_idom = reduce(intersect, (v for v in u.pred if v in idom))
            if u not in idom or idom[u] != new_idom:
                idom[u] = new_idom
                changed = True
    for key, value in idom.items():
        key.idom = value
    return idom


def calc_dominance_frontier(blocks: List[BasicBlock]):
    """
    Calculates Dominance Frontiers of the nodes.
    Immediate Dominators are expected to be precalculated.
    """

    for b in blocks:
        if len(b.pred) < 2:
            continue
        for p in b.pred:
            runner: BasicBlock = p
            while runner is not b.idom:
                runner.dominance_frontiers.append(b)
                runner = runner.idom


def dominance_frontier(ir: List[FnDef | FnDecl]):
    """Calculates Dominance Frontiers of the nodes"""

    for fn in ir:
        match fn:
            case FnDef():
                calc_idom(fn.basic_blocks[0], fn.basic_blocks)
                calc_dominance_frontier(fn.basic_blocks)
