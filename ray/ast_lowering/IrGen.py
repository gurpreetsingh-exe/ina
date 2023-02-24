from __future__ import annotations
from typing import Dict
from ..Ast import *
from ..ir.function import *
from ..ir.inst import *
from ..ir import inst


class Env:
    def __init__(self, parent: Env | None = None) -> None:
        self.parent = parent
        self.bindings: Dict[str, InstId] = {}

    def bind(self, name: str, inst: InstId):
        self.bindings[name] = inst

    def find(self, name: str) -> InstId:
        if name in self.bindings:
            return self.bindings[name]
        elif self.parent:
            return self.parent.find(name)
        else:
            assert False


class BasicBlockBuilder:
    def __init__(self, instructions: List[Inst]) -> None:
        self.instructions = instructions

    def link_blocks(self, basic_blocks: List[BasicBlock]):
        for basic_block in basic_blocks:
            if not basic_block.instructions:
                continue
            match basic_block.instructions[-1]:
                case Br(_, btrue, bfalse):
                    true_block = basic_blocks[btrue]
                    false_block = basic_blocks[bfalse]
                    basic_block.succ += [true_block, false_block]
                    true_block.pred.append(basic_block)
                    false_block.pred.append(basic_block)
                case Jmp(br_id):
                    jmp_block = basic_blocks[br_id]
                    basic_block.succ.append(jmp_block)
                    jmp_block.pred.append(basic_block)

    def run(self) -> List[BasicBlock]:
        basic_blocks: List[BasicBlock] = []
        basic_blocks.append(BasicBlock([], [], []))
        for inst in self.instructions:
            basic_blocks[-1].add(inst)
            match inst:
                case Br() | Jmp():
                    basic_blocks.append(BasicBlock([], [], []))
        self.link_blocks(basic_blocks)
        return basic_blocks


class IRGen:
    def __init__(self, ast) -> None:
        self.ast = ast
        self.off = 0
        self.env = Env()
        self.instructions = []
        self._bb_id = 0

    def add_inst(self, inst: Inst) -> InstId:
        self.instructions.append(inst)
        return InstId(inst.inst_id)

    @property
    def bb_id(self):
        self._bb_id += 1
        return self._bb_id

    def lower_expr(self, expr: Expr) -> Value:
        match expr.kind:
            case Ident(name):
                return self.add_inst(Load(self.env.find(name)))
            case If(cond, then, elze):
                c = self.lower_expr(cond)
                btrue = self.bb_id
                br_id = self.add_inst(Br(c, btrue, 0))
                self.lower_block(then)
                bfalse = self.bb_id
                self.instructions[br_id.i].bfalse = bfalse
                jmp = self.add_inst(Jmp(bfalse))
                if elze:
                    self.lower_block(elze)
                    bb_id = self.bb_id
                    self.instructions[jmp.i].br_id = bb_id
                    self.add_inst(Jmp(bb_id))
                return c
            case Literal():
                return inst.Const.from_lit(expr.kind)
            case _:
                assert False, expr.kind

    def lower_stmt(self, stmt: Stmt):
        match stmt.kind:
            case Let(name, ty, init):
                p = self.lower_expr(init)
                inst = self.add_inst(Alloc(ty, self.off))
                self.add_inst(Store(inst, p))
                self.env.bind(name, inst)
            case Expr(_):
                self.lower_expr(stmt.kind)
            case _:
                assert False

    def lower_block(self, block: Block):
        tmp = self.env
        self.env = Env(tmp)
        for stmt in block.stmts:
            self.lower_stmt(stmt)
        self.env = tmp

    def lower_fn(self, fn: Fn) -> FnDef | FnDecl:
        match fn:
            case Fn(name, args, ret_ty, body, is_extern, abi):
                ir_args = []
                for arg in args:
                    match arg:
                        case Arg(param, ty):
                            size = ty.get_size()
                            self.off = (self.off + size * 2 - 1) & ~(size - 1)
                            p = Inst(param)
                            ir_args.append(p)
                            if not is_extern:
                                inst = self.add_inst(
                                    Alloc(ty, self.off))
                                self.env.bindings[param] = inst
                                self.add_inst(Store(inst, p))
                        case Variadic():
                            ir_args.append(Inst("..."))
                ret = ret_ty if ret_ty else PrimTy(PrimTyKind.Unit)
                if is_extern:
                    return FnDecl(name, ir_args, ret)
                if body:
                    self.lower_block(body)
                basic_blocks = BasicBlockBuilder(self.instructions).run()
                self.instructions.clear()
                return FnDef(name, ir_args, ret, basic_blocks)

    def lower(self) -> List[FnDecl | FnDef]:
        lowered_fn = []
        for node in self.ast:
            match node:
                case Fn():
                    lowered_fn.append(self.lower_fn(node))
                case ExternBlock(fns):
                    for fn in fns:
                        lowered_fn.append(self.lower_fn(fn))
                case _:
                    assert False, node
        return lowered_fn
