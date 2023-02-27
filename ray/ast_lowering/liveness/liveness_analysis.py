from typing import Dict, List, Set
from itertools import chain
from ...ir.basic_block import BasicBlock
from ...ir.function import FnDef
from ...ir.inst import *
from ..dominance_frontier import postorder

"""
/// Partial liveness, with post-order traversal.

Function DAG_DFS(block B )
    foreach S ∈ succs(B ) such that (B ,S ) is not a back-edge do
        if S is unprocessed then DAG_DFS(S )
    Live ← PhiUses(B )
    foreach S ∈ succs(B ) such that (B ,S ) is not a back-edge do
        Live ← Live ∪ (LiveIn(S ) \\ PhiDefs(S ))
    LiveOut(B ) ← Live
    foreach program point p in B , backwards, do
        remove variables defined at p from Live
        add uses at p to Live
    LiveIn(B ) ← Live ∪ PhiDefs(B )
    mark B as processed
"""


def up_and_mark(block: BasicBlock, var: Inst):
    for inst in block.instructions:
        match inst:
            case Store():
                dst = inst.dst
                assert isinstance(dst, InstId)
                # killed in block
                if dst.i == var.inst_id:
                    return

    if var in block.live_ins:
        return
    block.live_ins.add(var)
    for p in block.pred:
        p.live_outs.add(var)
        up_and_mark(p, var)


class LiveInterval:
    def __init__(self) -> None:
        self.start: int
        self.end: int


def liveness(fn: FnDef):
    defs: Dict[BasicBlock, Set[Inst]] = {b: set() for b in fn.basic_blocks}
    for b in fn.basic_blocks:
        for i in b.instructions:
            match i:
                case Store() | Br() | Jmp():
                    continue
                case _:
                    defs[b].add(i)

    live_in: Dict[BasicBlock, Set[Inst]] = {b: set() for b in fn.basic_blocks}
    live_intervals: Dict[Inst, List[LiveInterval]] = {}
    """
    for v in chain(*defs.values()):
        for b in fn.basic_blocks:
            if v in defs[b]:
                continue
            for p in b.instructions:
                if p.uses(v):
                    live_in[b].add(v)
    """
    for v in chain(*defs.values()):
        for b in postorder(fn.basic_blocks[0]):
            if v in defs[b]:
                continue
            for p in b.instructions:
                if p.uses(v):
                    if v not in live_intervals:
                        live_intervals[v] = []
                        live_intervals[v].append(LiveInterval())
                    live_in[b].add(v)
                    live_intervals[v][-1].start = b.instructions.index(p)
    print(live_intervals)

    print("LiveIn:")
    for b, uses in live_in.items():
        b.live_ins = uses
        print("  LiveIn(bb{}) = {{{}}}".format(b.bb_id, ", ".join(
            map(lambda a: f"%{a.inst_id}", list(uses)))))

    live_out: Dict[BasicBlock, Set[Inst]] = {b: set() for b in fn.basic_blocks}
    """
    for v in chain(*defs.values()):
        for b in fn.basic_blocks:
            for p in postorder(b):
                if p == b or v in defs[p]:
                    continue
                if p.uses(v):
                    if v.inst_id == 17:
                        print(p.bb_id, v)
                    live_out[b].add(v)
    """
    for v in chain(*defs.values()):
        for b in postorder(fn.basic_blocks[0]):
            for p in b.succ:
                if p == b or v in defs[p]:
                    continue
                if p.uses(v):
                    live_out[b].add(v)
    print("\nLiveOut:")
    for b, uses in live_out.items():
        b.live_outs = uses
        print("  LiveOut(bb{}) = {{{}}}".format(b.bb_id, ", ".join(
            map(lambda a: f"%{a.inst_id}", list(uses)))))


def compute_liveness_sets(basic_blocks: List[BasicBlock]):
    for basic_block in basic_blocks:
        uses = [i for i in basic_block.instructions if basic_block.uses(i)]
        for v in uses:
            up_and_mark(basic_block, v)

        print("  LiveIn(bb{}) = {{{}}}".format(basic_block.bb_id, ", ".join(
            map(lambda a: f"%{a.inst_id}", basic_block.live_ins))))
        print("  LiveOut(bb{}) = {{{}}}".format(basic_block.bb_id, ", ".join(
            map(lambda a: f"%{a.inst_id}", basic_block.live_outs))))


def dag_dfs(block: BasicBlock):
    blocks = postorder(block)
    blocks.remove(block)
    for s in blocks:
        if s.processed:
            continue
        dag_dfs(s)

    live = set()
    for b in blocks:
        if b.processed:
            continue
        live_in = set([i for i in b.instructions if b.uses(i)])
        print(live_in)
        live.add(live_in)

    print("  LiveOut(bb{}) = {{{}}}".format(block.bb_id, ", ".join(
        map(lambda a: f"%{a.inst_id}", list(live)))))

    block.processed = True
