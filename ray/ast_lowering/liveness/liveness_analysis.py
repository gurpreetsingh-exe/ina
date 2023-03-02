from typing import Dict, List, Set, Tuple
from itertools import chain
from ...ir.basic_block import BasicBlock
from ...ir.function import FnDef
from ...ir.inst import *
from ..dominance_frontier import postorder


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
    for v in chain(*defs.values()):
        for b in postorder(fn.basic_blocks[0]):
            if v in defs[b]:
                continue
            for p in b.instructions:
                if p.uses(v):
                    live_in[b].add(v)

    print("LiveIn:")
    for b, uses in live_in.items():
        b.live_ins = uses
        print("  LiveIn(bb{}) = {{{}}}".format(b.bb_id, ", ".join(
            map(lambda a: f"%{a.inst_id}", list(uses)))))

    live_out: Dict[BasicBlock, Set[Inst]] = {b: set() for b in fn.basic_blocks}
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


class LargeBlockInfo:
    def __init__(self) -> None:
        self.inst_index: Dict[Inst, int] = {}

    def get_inst_index(self, inst: Inst) -> int:
        if inst in self.inst_index:
            return self.inst_index[inst]
        for i, bbi in enumerate(inst.parent.instructions):
            match bbi:
                case Load() | Store():
                    self.inst_index[bbi] = i
        assert inst in self.inst_index, "insertion failed"
        return self.inst_index[inst]


class AllocInfo:
    def __init__(self) -> None:
        # Blocks where alloc is used
        self.using: List[BasicBlock] = []
        # Blocks where alloc is defined
        self.defs: List[BasicBlock] = []

        self.only_used_in_one_block = True
        self.only_block: BasicBlock | None = None
        self.only_store: Store | None = None

    def analyze(self, alloc: Alloc, fn: FnDef):
        users = set()
        for basic_block in fn.basic_blocks:
            for i in alloc.users(basic_block):
                users.add(i)
        alloc.userz += users

        for user in users:
            match user:
                case Load():
                    if user.parent not in self.using:
                        self.using.append(user.parent)
                case Store():
                    if user.parent not in self.defs:
                        self.defs.append(user.parent)
                        self.only_store = user
            if self.only_used_in_one_block:
                if not self.only_block:
                    self.only_block = user.parent
                elif self.only_block != user.parent:
                    self.only_used_in_one_block = False


def rewrite_single_store_alloc(alloc: Alloc, info: AllocInfo, large_block_info: LargeBlockInfo):
    only_store = info.only_store
    assert only_store != None
    store_block = only_store.parent
    store_index = -1
    info.using.clear()

    for user_inst in alloc.userz:
        if user_inst == only_store:
            continue
        load_inst = user_inst
        if load_inst.parent:
            if store_index == -1:
                store_index = large_block_info.get_inst_index(only_store)

            if store_index > large_block_info.get_inst_index(load_inst):
                info.using.append(store_block)
                continue
        elif not store_block.dominates(load_inst.parent):
            info.using.append(load_inst.parent)
            continue

        repl_val = only_store.dst
        if repl_val == load_inst:
            repl_val = PoisonValue()
    return False


def promote_single_block_alloc(alloc: Alloc, info: AllocInfo, large_block_info: LargeBlockInfo):
    store_by_index: List[Tuple[int, Store]] = []
    for user in alloc.userz:
        match user:
            case Store():
                store_by_index.append(
                    (large_block_info.get_inst_index(user), user))
    for user in alloc.userz:
        load = user
        if not isinstance(load, Load):
            continue
        load_index = large_block_info.get_inst_index(load)
    return True


def compute_livein_blocks(alloc: Alloc, info: AllocInfo, defs: List[BasicBlock], live_ins: List[BasicBlock]):
    live_in_work_list = list(info.defs)
    i = 0
    while i != len(live_in_work_list):
        bb = live_in_work_list[i]
        if bb not in defs:
            i += 1
            continue
        for inst in bb.instructions:
            match inst:
                case Store():
                    if inst.src != alloc:
                        continue
                    live_in_work_list[i] = live_in_work_list[-1]
                    live_in_work_list.pop()
                    i -= 1
                case Load():
                    if inst.src == alloc:
                        break
        i += 1

    while live_in_work_list:
        bb = live_in_work_list.pop()
        if bb not in live_ins:
            live_ins.append(bb)
            continue
        for p in bb.pred:
            if p not in defs:
                continue
            live_in_work_list.append(p)


def get_phi_blocks(defs: List[BasicBlock], live_ins: List[BasicBlock]) -> List[BasicBlock]:
    W: Set[BasicBlock] = set(defs)
    F: Set[BasicBlock] = set()
    while W:
        X = W.pop()
        for Y in X.dominance_frontiers:
            F.add(Y)
    return list(F)


def queue_phi_node(bb: BasicBlock, alloc: Alloc, current_version: int):
    # bb.instructions.insert(0, Phi(bb.bb_id, alloc.parent.bb_id))
    pass


def promote_mem_to_reg(fn: FnDef, allocs: List[Alloc]):
    info = AllocInfo()
    large_block_info = LargeBlockInfo()
    alloc_lookup: Dict[Alloc, int] = {}
    for i, alloc in enumerate(allocs):
        info.analyze(alloc, fn)
        if not alloc.userz:
            alloc.remove_from_parent()
            allocs.remove(alloc)
            continue
        if len(info.defs) == 1:
            if rewrite_single_store_alloc(alloc, info, large_block_info):
                allocs.remove(alloc)
                continue
        if info.only_used_in_one_block and promote_single_block_alloc(alloc, info, large_block_info):
            allocs.remove(alloc)
            continue

        alloc_lookup[alloc] = i
        defs: List[BasicBlock] = list(info.defs)
        live_ins = []
        compute_livein_blocks(alloc, info, defs, live_ins)
        current_version = 0
        for bb in get_phi_blocks(defs, live_ins):
            queue_phi_node(bb, alloc, current_version)
    if not allocs:
        return
    for alloc in allocs:
        if alloc.userz:
            for user in alloc.userz:
                bb = user.parent
                bb.instructions.remove(user)
        alloc.remove_from_parent()


def mem2reg(fn: FnDef):
    allocs: List[Alloc] = []
    basic_block: BasicBlock = fn.basic_blocks[0]
    for basic_block in fn.basic_blocks:
        for inst in basic_block.instructions:
            match inst:
                case Alloc():
                    allocs.append(inst)
    promote_mem_to_reg(fn, allocs)
