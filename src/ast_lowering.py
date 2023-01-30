from typing import Dict
from Ast import *
from ir import *


class LoweringContext:
    def __init__(self, ast: List[Fn]) -> None:
        self.ast = ast
        self.fn: Fn | None = None
        self.lowered_ast = []
        self.bb_id = None
        self.bb: BasicBlock | None = None
        self.blocks: List[BasicBlock] = []
        self.inst_id: int = 0
        self.str_id: int = 0
        self.strings = []

    def reset(self):
        self.fn: Fn | None = None
        self.bb_id = None
        self.bb: BasicBlock | None = None
        self.blocks: List[BasicBlock] = []
        self.inst_id: int = 0

    def alloc_str(self, val: str) -> Value:
        self.strings.append(val)
        self.str_id += 1
        return Value(ValueKind.SymbolId, self.str_id)

    def mk_inst(self, inst) -> Value:
        assert self.bb != None
        val = Value(ValueKind.InstId, self.inst_id)
        self.bb.instructions.append(
            Instruction(inst, self.inst_id))
        self.inst_id += 1
        return val

    def mk_basic_block(self):
        parent_bb = self.bb_id
        if self.bb_id == None:
            self.bb_id = 0
        else:
            self.bb_id += 1
        self.bb = BasicBlock([], parent_bb, self.bb_id)

    def alloc_var(self, name: str, val: Value):
        assert self.bb != None
        self.bb.locals[name] = val

    def get_var(self, name) -> Value:
        assert self.bb != None
        if name in self.bb.locals:
            return self.bb.locals[name]
        elif self.bb.parent != None:
            paren_id = self.bb.parent
            while paren_id != None:
                parent = self.blocks[paren_id]
                if val := parent.get_var(name):
                    return val
                else:
                    paren_id = parent.parent
            assert False
        else:
            assert False

    def append_bb(self):
        assert self.bb != None
        self.blocks.append(self.bb)


class IRGen:
    def __init__(self, ctx: LoweringContext) -> None:
        self.ctx = ctx
        self.label_id = 0
        self.reg_id = 0
        for fn in self.ctx.ast:
            self.ctx.lowered_ast.append(self.lower_fn(fn))

    def reset(self):
        self.label_id = 0
        self.reg_id = 0
        self.ctx.reset()

    @property
    def label(self):
        i = self.label_id
        self.label_id += 1
        return i

    def lower_expr(self, expr: Expr) -> Value:
        assert self.ctx.bb_id != None
        match expr.kind:
            case Assign(Ident(name), init):
                ptr = self.ctx.get_var(name)
                val = self.lower_expr(init)
                return self.ctx.mk_inst(Store(ptr, val))
            case Call(name, args):
                args = [self.lower_expr(arg) for arg in args]
                return self.ctx.mk_inst(FnCall(name, args))
            case Binary(kind, left, right):
                l = self.lower_expr(left)
                r = self.lower_expr(right)
                cls = None
                match kind:
                    case BinaryKind.Add:
                        cls = Add
                    case BinaryKind.Sub:
                        cls = Sub
                    case BinaryKind.Mod:
                        cls = Mod
                    case BinaryKind.Lt | BinaryKind.Gt:
                        return self.ctx.mk_inst(Cmp(l, r, CmpKind.from_binary(kind)))
                    case _:
                        assert False, f"{kind}"
                assert cls != None
                return self.ctx.mk_inst(cls(l, r))
            case Literal(kind, value):
                assert expr.ty != None
                match kind:
                    case Lit.Str:
                        src = self.ctx.alloc_str(value)
                        return src
                    case Lit.Int | Lit.Bool | Lit.Float:
                        return Value(ValueKind.Imm, value)
                    case _:
                        assert False, kind
            case Ident(name):
                ptr = self.ctx.get_var(name)
                return self.ctx.mk_inst(Load(ptr))
            case If(cond, true_block, false_block):
                c = self.lower_expr(cond)
                btrue = self.ctx.bb_id + 1
                end = self.ctx.bb_id + 2
                bfalse = 0
                if false_block:
                    bfalse = self.ctx.bb_id + 2
                    end = self.ctx.bb_id + 3
                else:
                    bfalse = self.ctx.bb_id + 2
                self.ctx.mk_inst(Br(c, btrue, bfalse))
                self.ctx.append_bb()
                self.lower_block(true_block)
                if false_block:
                    self.ctx.mk_inst(Jmp(end))
                    self.lower_block(false_block)
                self.ctx.mk_basic_block()
                return c
            case Unary(kind, expr):
                inst_id = self.lower_expr(expr)
                match kind:
                    case UnaryKind.Not:
                        return self.ctx.mk_inst(Sub(Value(ValueKind.Imm, 1), inst_id))
                    case _:
                        assert False
            case _:
                assert False, f"{expr.kind}"

    def lower_args(self):
        assert self.ctx.fn != None
        for arg in self.ctx.fn.args:
            match arg:
                case Arg(name, ty):
                    ptr = self.ctx.mk_inst(Alloc(ty))
                    self.ctx.alloc_var(name, ptr)

    def lower_block(self, block: Block):
        self.ctx.mk_basic_block()
        if self.ctx.fn:
            self.lower_args()
            self.ctx.fn = None

        for stmt in block.stmts:
            match stmt.kind:
                case Expr():
                    self.lower_expr(stmt.kind)
                case Let(name, ty, init):
                    val = self.lower_expr(init)
                    ptr = self.ctx.mk_inst(Alloc(ty))
                    self.ctx.alloc_var(name, ptr)
                    self.ctx.mk_inst(Store(ptr, val))
                case _:
                    assert False, f"{stmt.kind}"
        self.ctx.append_bb()

    def lower_fn(self, fn: Fn) -> FnDecl | FnDef:
        match fn:
            case Fn(name, _, ret_ty, body, is_extern, abi):
                self.ctx.fn = fn
                ir_args = []
                ret = ret_ty if ret_ty else PrimTy(PrimTyKind.Unit)
                if is_extern:
                    return FnDecl(name, ir_args, ret)

                blocks = None
                if body:
                    self.lower_block(body)
                blocks = self.ctx.blocks
                assert blocks != None
                self.reset()
                return FnDef(name, ir_args, ret, blocks)
