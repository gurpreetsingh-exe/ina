from __future__ import annotations
from ..Ast import *
from ..Token import *
from ..utils import panic
from ..Errors import *
# from beeprint import pp
# from copy import deepcopy


class DefKind(Enum):
    Fn = auto()
    Struct = auto()


class Def:
    def __init__(self, kind, data):
        self.kind = kind
        self.data = data


class TyEnv:
    def __init__(self, parent: TyEnv | None = None):
        self.parent = parent
        self.bindings = {}

    def search_local(self, name) -> bool:
        if name in self.bindings:
            return True
        elif self.parent:
            return self.parent.search_local(name)
        else:
            return False

    def find_local(self, name) -> Ty:
        if name in self.bindings:
            return self.bindings[name]['ty']
        elif self.parent:
            return self.parent.find_local(name)
        assert False, name

    def def_local(self, name, ty, val=None):
        if self.search_local(name):
            panic(f"{name} is already defined")
        self.bindings[name] = {'name': name, 'ty': ty, 'val': val}


class TyCheck:
    def __init__(self, ast: Module, file: File):
        self.file = file
        self.ast = ast
        self.defs = {}
        self.env = None
        self.fn_ctx = None
        self.errors = []
        self.consts = {}
        self.globls = {}
        self.types = {}
        self.tychk()

    def mk_bool(self) -> PrimTy:
        return PrimTy(PrimTyKind.Bool)

    def mk_unit(self) -> PrimTy:
        return PrimTy(PrimTyKind.Unit)

    def mk_prim_ty(self, kind) -> PrimTy:
        return PrimTy(kind)

    def add_err(self, err: Error, span: Span | None) -> Error:
        err.span = span
        self.errors.append(err)
        return err

    def infer(self, expr: Expr) -> Ty:
        span = expr.span
        match expr:
            case Assign(Ident(name), init):
                if not self.env.search_local(name):
                    self.add_err(NotFound(name, ""), span)
                ty = self.env.find_local(name)
                expr.ty = ty
                inf_ty = self.infer(init)
                if inf_ty != ty:
                    self.add_err(TypesMismatchError(
                        f"expected `{ty}`, found `{inf_ty}`"), span)
                return ty
            case Binary(kind, left, right):
                lty = self.infer(left)
                rty = self.infer(right)
                if lty == rty:
                    match kind:
                        case BinaryKind.Lt | BinaryKind.Gt | BinaryKind.Eq | BinaryKind.NotEq:
                            expr.ty = self.mk_bool()
                            return self.mk_bool()
                    expr.ty = lty
                    return lty
                else:
                    self.add_err(TypesMismatchError(
                        f"cannot {kind} `{lty}` and `{rty}`"), span)
                    return lty
            case Literal(kind, _):
                val = None
                match kind:
                    case Lit.Int:
                        val = PrimTyKind.I64
                    case Lit.Float:
                        val = PrimTyKind.F64
                    case Lit.Bool:
                        val = PrimTyKind.Bool
                    case Lit.Str:
                        val = PrimTyKind.Str
                    case Lit.Char:
                        val = PrimTyKind.Char
                    case _:
                        panic(f"unexpected literal {kind}")
                ty = self.mk_prim_ty(val)
                expr.ty = ty
                return ty
            case Call(name, args):
                if ty := self.check_call(name, args, span):
                    return ty
                else:
                    return self.mk_unit()
            case Ident(name):
                if name in self.consts:
                    expr.ty = self.consts[name]
                    return self.consts[name]
                elif name in self.globls:
                    return self.globls[name]
                elif self.env.search_local(name):
                    ty = self.env.find_local(name)
                    expr.ty = ty
                    return ty
                elif name in self.defs:
                    deff = self.defs[name]
                    match deff.kind:
                        case DefKind.Fn:
                            fn = deff.data
                            args = [arg.ty for arg in fn['args']]
                            ty = FnTy(args, fn['ret_ty'])
                            expr.ty = ty
                            return ty
                        case _:
                            assert False
                else:
                    self.add_err(NotFound(name, ""), span).emit(
                        self.file, True)
            case Unary(kind, expr):
                match kind:
                    case UnaryKind.Not:
                        return self.infer(expr)
                    case UnaryKind.Neg:
                        ty = self.infer(expr)
                        match ty:
                            case PrimTy(kind):
                                if kind.is_number():
                                    return ty
                                else:
                                    panic(f"expected number but got {ty}")
                            case _:
                                assert False
                    case UnaryKind.AddrOf:
                        if type(expr) != Ident:
                            panic(
                                f"cannot use & on {expr.__class__.__name__}")
                        ty = self.infer(expr)
                        return RefTy(ty)
                    case UnaryKind.Deref:
                        match ty := self.infer(expr):
                            case PtrTy(ptr):
                                return ptr
                            case _:
                                self.add_err(DerefError(ty), span)
                    case _:
                        assert False, f"{kind} unreachable"
            case If(cond, body, elze):
                self.check(cond, self.mk_bool())
                expr.cond.ty = self.mk_bool()
                if_ty = self.infer_block(body)
                else_ty = None
                if elze:
                    else_ty = self.infer_block(elze)
                    assert if_ty == else_ty
                return if_ty
            case Loop(body):
                self.infer_block(body)
                return self.mk_unit()
            case Cast(expr, ty):
                expr.ty = self.infer(expr)
                return ty
            case StructExpr(name, fields):
                if name not in self.types:
                    self.add_err(NotFound(name, ""), span)
                    return
                struct = self.types[name]
                field_tys = []
                defined_fields = []
                for struct_field in struct.fields:
                    for expr_field in fields:
                        if expr_field.name not in defined_fields:
                            defined_fields.append(expr_field.name)
                        if expr_field.name == struct_field.name:
                            self.check(expr_field.expr, struct_field.ty)
                            field_tys.append(struct_field.ty)
                            break
                undefined_fields = set(
                    [f.name for f in struct.fields]) - set(defined_fields)
                if undefined_fields:
                    for f in undefined_fields:
                        self.add_err(MissingStructFieldError(
                            f"missing `{f}`", f, name), span)
                return StructTy(name, field_tys)
            case ArrayRepeat(item, length):
                return ArrayTy(self.infer(item), length)
            case ArrayNor(items):
                items_ty = [self.infer(item) for item in items]
                if not items_ty:
                    self.add_err(MissingTyAnnError(
                        "type annotation required"), span).emit(self.file, True)
                first = items_ty[0]
                for item in items:
                    ty = self.infer(item)
                    if first != ty:
                        self.add_err(TypesMismatchError(
                            f"expected `{first}`, found `{ty}`"), item.span).emit(self.file, True)
                return ArrayTy(first, len(items_ty))
            case _:
                assert False, f"{expr}"

    def check(self, expr: Expr, expected_ty: Ty):
        span = expr.span
        match expr:
            case Assign(Ident(name), init):
                if not self.env.search_local(name):
                    self.add_err(NotFound(name, ""), span)
                ty = self.env.find_local(name)
                expr.ty = ty
                self.check(init, expected_ty)
            case Binary(kind, left, right):
                match kind:
                    case BinaryKind.Lt | BinaryKind.Gt | BinaryKind.Eq | BinaryKind.NotEq:
                        if expected_ty != self.mk_bool():
                            self.add_err(TypesMismatchError(
                                f"expected `bool`, found `{expected_ty}`"), span)
                        lty = self.infer(left)
                        rty = self.infer(right)
                        if lty != rty:
                            self.add_err(TypesMismatchError(
                                f"cannot {kind} `{lty}` and `{rty}`"), span)
                    case _:
                        self.check(left, expected_ty)
                        self.check(right, expected_ty)
            case Literal(kind, _):
                match expected_ty:
                    case PrimTy(ty_kind):
                        if kind is Lit.Int and ty_kind.is_int():
                            pass
                        elif kind is Lit.Float and ty_kind.is_float():
                            pass
                        elif kind is Lit.Str and ty_kind == PrimTyKind.Str:
                            pass
                        elif kind is Lit.Bool and ty_kind == PrimTyKind.Bool:
                            pass
                        else:
                            self.add_err(TypesMismatchError(
                                f"expected `{expected_ty}`, found `{kind.name.lower()}`"), span)
                expr.ty = expected_ty
            case Call(name, args):
                self.check_call(name, args, span, expected_ty)
            case Ident(name):
                if name in self.consts:
                    return self.consts[name]
                elif name in self.globls:
                    return self.globls[name]
                elif self.env.search_local(name):
                    ty = self.env.find_local(name)
                    if ty != expected_ty:
                        self.add_err(TypesMismatchError(
                            f"expected `{expected_ty}`, found `{ty}`"), span)
                    expr.ty = ty
                elif name in self.defs:
                    deff = self.defs[name]
                    match deff.kind:
                        case DefKind.Fn:
                            fn = deff.data
                            if type(expected_ty) != FnTy:
                                self.add_err(TypesMismatchError(
                                    f"`{name}` has type function"), span)
                            args = [arg.ty for arg in fn['args']]
                            found = FnTy(args, fn['ret_ty'])
                            if found != expected_ty:
                                self.add_err(TypesMismatchError(
                                    f"expected `{expected_ty}`, found `{found}`"), span)
                        case _:
                            assert False
                else:
                    panic(f"{name} is not defined")
            case Unary(kind, expr):
                match kind:
                    case UnaryKind.Not:
                        self.check(expr, expected_ty)
                    case UnaryKind.Neg:
                        match expected_ty:
                            case PrimTy(kind):
                                if kind.is_number():
                                    self.check(expr, expected_ty)
                                else:
                                    self.add_err(TypesMismatchError(
                                        f"expected `{expected_ty}`, found number"), span)
                    case UnaryKind.AddrOf:
                        if type(expr) != Ident:
                            panic(
                                f"cannot use & on {expr.__class__.__name__}")
                        match expected_ty:
                            case RefTy(ty):
                                self.check(expr, ty)
                            case _:
                                ty = RefTy(self.infer(expr))
                                self.add_err(TypesMismatchError(
                                    f"expected `{expected_ty}`, found {ty}"), span)
                    case UnaryKind.Deref:
                        self.check(expr, PtrTy(expected_ty))
                    case _:
                        assert False, f"{kind} unreachable"
            case If(cond, body, elze):
                self.check(cond, self.mk_bool())
                self.check_block(body, expected_ty)
                if elze:
                    self.check_block(elze, expected_ty)
            case Loop(body):
                self.check_block(body, expected_ty)
            case Cast(cast_expr, ty):
                cast_expr.ty = self.infer(cast_expr)
                match cast_expr.ty, ty:
                    case RefTy(_) | PrimTy(PrimTyKind.Raw) | FnTy(), PrimTy(PrimTyKind.Raw):
                        pass
                    case PrimTy(_), PrimTy(PrimTyKind.Raw):
                        self.add_err(CastError(
                            f"invalid cast of `{cast_expr.ty}`"), span)
                    case PrimTy(p), RefTy(_) if p != PrimTyKind.Raw:
                        self.add_err(CastError(
                            f"invalid cast of `{cast_expr.ty}`"), span)
                    case PtrTy(lty), RefTy(rty):
                        if lty != rty:
                            self.add_err(CastError(
                                f"invalid cast of `{cast_expr.ty}`"), span)
                    case RefTy(rty), PtrTy(pty):
                        if rty != pty:
                            self.add_err(CastError(
                                f"invalid cast of `{cast_expr.ty}`"), span)
                    case PrimTy(_), PrimTy(_):
                        pass
                    case PtrTy(_), PrimTy(PrimTyKind.Str | PrimTyKind.Raw):
                        pass
                    case PrimTy(PrimTyKind.Raw), PtrTy(_):
                        pass
                    case _, _:
                        self.add_err(CastError(
                            f"invalid cast of `{cast_expr.ty}`"), span)
                if ty != expected_ty:
                    self.add_err(TypesMismatchError(
                        f"expected `{expected_ty}`, found `{ty}`"), span)
                expr.ty = ty
            case ArrayRepeat(item, length):
                match expected_ty:
                    case ArrayTy(ty, _len):
                        if length != _len:
                            self.add_err(TypesMismatchError(
                                f"expected `{expected_ty}`, found `{ty}`"), span).emit(self.file, True)
                        self.check(item, ty)
                    case _:
                        ty = ArrayTy(self.infer(item), length)
                        self.add_err(TypesMismatchError(
                            f"expected `{expected_ty}`, found `{ty}`"), span).emit(self.file, True)
            case _:
                assert False, expr

    def check_call(self, name, args, span, expected_ty=None):
        defn = self.defs.get(name)
        expected_args = None
        expected_ret_ty = None
        if defn != None:
            expected_args = [arg.ty for arg in defn.data['args']]
            expected_ret_ty = defn.data['ret_ty']
        elif self.env.search_local(name):
            ty = self.env.find_local(name)
            match ty:
                case FnTy(fargs, ret_ty):
                    expected_args = fargs
                    expected_ret_ty = ret_ty
                case _:
                    self.add_err(TypesMismatchError(
                        f"`{name}` is not a function"), span)
                    return self.mk_unit()
        else:
            self.add_err(NotFound(name, ""), span)
            return self.mk_unit()
        variadic = False
        starts_at = 0
        for arg in expected_args:
            if type(arg) == VariadicTy:
                variadic = True
                break
            starts_at += 1

        found = len(args)
        expected = len(expected_args)

        if not variadic and found != expected:
            panic(f"expected {expected} args found {found}")

        if variadic:
            for i, arg in enumerate(args):
                if i >= starts_at:
                    arg.ty = self.infer(arg)
                else:
                    self.check(arg, expected_args[i])
                    arg.ty = expected_args[i]
        else:
            for arg, exp in zip(args, expected_args):
                self.check(arg, exp)
                arg.ty = exp

        if expected_ty and expected_ret_ty != expected_ty:
            panic(
                f"unexpected type: expected {expected_ty} found {expected_ret_ty}")
        return expected_ret_ty

    def check_block(self, block, expected_ty):
        tmp_env = self.env
        self.env = TyEnv(tmp_env)
        if self.fn_ctx:
            deff = self.defs[self.fn_ctx]
            assert deff.kind == DefKind.Fn
            fn_dict = deff.data
            for arg in fn_dict['args']:
                if type(arg) == Variadic:
                    continue
                self.env.def_local(arg.name, arg.ty)
        self.fn_ctx = None
        for stmt in block.stmts:
            self.visit_stmt(stmt)
        if block.expr:
            self.check(block.expr, expected_ty)
        self.env = tmp_env

    def infer_block(self, block):
        tmp_env = self.env
        self.env = TyEnv(tmp_env)
        if self.fn_ctx:
            deff = self.defs[self.fn_ctx]
            assert deff.kind == DefKind.Fn
            fn_dict = deff.data
            for arg in fn_dict['args']:
                if type(arg) == Variadic:
                    continue
                self.env.def_local(arg.name, arg.ty)
        self.fn_ctx = None
        for stmt in block.stmts:
            self.visit_stmt(stmt)
        if block.expr:
            return self.infer(block.expr)
        self.env = tmp_env
        return self.mk_unit()

    def visit_stmt(self, stmt: Stmt):
        span = stmt.span
        match stmt.kind:
            case Let(name, ty, init):
                if self.env.search_local(name):
                    self.add_err(Redefinition(f"{name} is not defined"), span)
                if ty:
                    self.check(init, ty)
                else:
                    ty = self.infer(init)
                    stmt.kind.ty = ty
                self.env.def_local(name, ty)
            case Break():
                pass
            case Return(expr):
                self.infer(expr)
            case _:
                self.infer(stmt.kind)

    def visit_block(self, block: Block):
        tmp_env = self.env
        self.env = TyEnv(tmp_env)
        if self.fn_ctx:
            deff = self.defs[self.fn_ctx]
            assert deff.kind == DefKind.Fn
            fn_dict = deff.data
            for arg in fn_dict['args']:
                if type(arg) == Variadic:
                    continue
                self.env.def_local(arg.name, arg.ty)
        self.fn_ctx = None
        for stmt in block.stmts:
            self.visit_stmt(stmt)
        if block.expr:
            self.infer(block.expr)
        self.env = tmp_env

    def visit_fn(self, fn: Fn):
        name = fn.name
        args = fn.args
        ret_ty = fn.ret_ty
        abi = fn.abi
        body = fn.body

        fn_dict = {'args': args, 'ret_ty': ret_ty, 'abi': abi}
        self.defs[name] = Def(DefKind.Fn, fn_dict)
        if fn.is_extern:
            return
        self.fn_ctx = name
        assert body != None
        self.visit_block(body)

    def tychk(self):
        for node in self.ast:
            span = node.span
            match node:
                case Fn():
                    self.visit_fn(node)
                case ExternBlock(items):
                    for fn in items:
                        self.visit_fn(fn)
                case Const(name, ty, init):
                    if name in self.consts:
                        self.add_err(Redefinition(
                            f"{name} is already defined"), span)
                    if ty:
                        self.check(init, ty)
                    else:
                        ty = self.infer(init)
                        node.ty = ty
                    self.consts[name] = ty
                case Let(name, ty, init):
                    if name in self.globls:
                        self.add_err(Redefinition(
                            f"{name} is already defined"), span)
                    if ty:
                        self.check(init, ty)
                    else:
                        ty = self.infer(init)
                        node.ty = ty
                    self.globls[name] = ty
                case Struct(name, _):
                    if name in self.types:
                        assert False
                    offset = 0
                    for field in reversed(node.fields):
                        field.offset = offset
                        size = field.ty.get_size()
                        offset += ((size + 3) // 4) * 4
                    self.types[name] = node
                case _:
                    assert False, f"{node}"
