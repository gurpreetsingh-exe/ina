from ..Ast import *
from ..ir.function import *
from ..ir.inst import *


class IRGen:
    def __init__(self, ast) -> None:
        self.ast = ast
        self.off = 0

    def lower_block(self, block: Block, r_basic_blocks: List[BasicBlock]):
        pass

    def lower_fn(self, fn: Fn) -> FnDef | FnDecl:
        match fn:
            case Fn(name, args, ret_ty, body, is_extern, abi):
                ir_args = []
                basic_blocks = []
                if not is_extern:
                    basic_blocks.append(BasicBlock([]))
                for arg in args:
                    match arg:
                        case Arg(param, ty):
                            size = ty.get_size()
                            self.off = (self.off + size * 2 - 1) & ~(size - 1)
                            p = Inst(param)
                            ir_args.append(p)
                            if not is_extern:
                                inst = basic_blocks[-1].add_inst(
                                    Alloc(ty, self.off))
                                basic_blocks[-1].add_inst(Store(inst, p))
                        case Variadic():
                            ir_args.append(Inst("..."))
                ret = ret_ty if ret_ty else PrimTy(PrimTyKind.Unit)
                if is_extern:
                    return FnDecl(name, ir_args, ret)
                if body:
                    self.lower_block(body, basic_blocks)
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
