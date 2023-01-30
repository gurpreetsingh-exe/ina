from typing import Dict
from Ast import *
from ir import *


class LoweringContext:
    def __init__(self, ast: List[Fn]) -> None:
        self.ast = ast
        self.fn: Fn | None = None
        self.lowered_ast = []
        self.bb_id = 0
        self.bb: BasicBlock | None = None
        self.blocks: List[BasicBlock] = []
        self.inst_id: int = 0
        self.str_id: int = 0
        self.strings = []

    def alloc_str(self, val: str) -> Value:
        self.strings.append(val)
        self.str_id += 1
        return Value(ValueKind.SymbolId, self.str_id)

    def mk_inst(self, inst) -> Value:
        assert self.bb != None
        val = Value(ValueKind.InstId, self.inst_id)
        self.bb.instructions.append(
            Instruction(inst, len(self.bb.instructions)))
        self.inst_id += 1
        return val

    def mk_basic_block(self):
        parent_bb = self.bb_id
        self.bb = BasicBlock([], parent_bb)
        self.inst_id = 0
        self.bb_id += 1

    def store_var(self, name: str, val: Value):
        assert self.bb != None
        self.bb.locals[name] = val

    def get_var(self, name) -> Value:
        assert self.bb != None
        return self.bb.locals[name]

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

    @property
    def label(self):
        i = self.label_id
        self.label_id += 1
        return i

    def lower_expr(self, expr: Expr) -> Value:
        match expr.kind:
            case Assign(Ident(name), init):
                ptr = self.ctx.get_var(name)
                val = self.lower_expr(init)
                self.ctx.store_var(name, val)
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
            case _:
                assert False, f"{expr.kind}"

    def lower_fn_block(self, body: Block) -> List[BasicBlock]:
        self.ctx.mk_basic_block()
        for stmt in body.stmts:
            match stmt.kind:
                case Expr():
                    self.lower_expr(stmt.kind)
                case Let(name, ty, init):
                    val = self.lower_expr(init)
                    ptr = self.ctx.mk_inst(Alloc(ty))
                    self.ctx.store_var(name, ptr)
                    self.ctx.mk_inst(Store(ptr, val))
                case _:
                    assert False, f"{stmt.kind}"
        self.ctx.append_bb()
        return self.ctx.blocks

    def lower_fn(self, fn: Fn) -> FnDecl | FnDef:
        match fn:
            case Fn(name, args, ret_ty, body, is_extern, abi):
                self.ctx.fn = fn
                ir_args = []
                ret = ret_ty if ret_ty else PrimTy(PrimTyKind.Unit)
                if is_extern:
                    return FnDecl(name, ir_args, ret)

                blocks = None
                if body:
                    blocks = self.lower_fn_block(body)
                assert blocks != None
                return FnDef(name, ir_args, ret, blocks)
