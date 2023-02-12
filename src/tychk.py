from __future__ import annotations
from Ast import *
from Token import *
from utils import panic
from Errors import *
from beeprint import pp


class DefKind(Enum):
    Fn = auto()
    Struct = auto()


class Def:
    def __init__(self, kind, data):
        self.kind = kind
        self.data = data


class TyScope:
    def __init__(self):
        self.scopes = []

    @property
    def stack_offset(self):
        self.scopes[-1]

    def search_local(self, name) -> bool:
        for scope in reversed(self.scopes):
            for var in scope:
                if var['name'] == name:
                    return True
        return False

    def find_local(self, name) -> Ty | None:
        for scope in reversed(self.scopes):
            for var in scope:
                if var['name'] == name:
                    return var['ty']

    def def_local(self, name, ty, val=None):
        if self.search_local(name):
            panic(f"{name} is already defined")
        self.scopes[-1].append({'name': name, 'ty': ty, 'val': val})

    def add(self):
        self.scopes.append([])

    def pop(self):
        self.scopes.pop()


class TyCheck:
    def __init__(self, ast):
        self.ast = ast
        self.defs = {}
        self.scope = TyScope()
        self.fn_ctx = None
        self.errors = []
        self.tychk()

    def mk_bool(self) -> PrimTy:
        return PrimTy(PrimTyKind.Bool)

    def mk_unit(self) -> PrimTy:
        return PrimTy(PrimTyKind.Unit)

    def mk_prim_ty(self, kind) -> PrimTy:
        return PrimTy(kind)

    def add_err(self, err: Error, span: Span):
        err.span = span
        self.errors.append(err)

    def infer(self, expr: Expr) -> Ty:
        span = expr.span
        match expr.kind:
            case Assign(Ident(name), init):
                if not self.scope.search_local(name):
                    self.add_err(NotFound(name, ""), span)
                ty = self.scope.find_local(name)
                expr.ty = ty
                inf_ty = self.infer(init)
                if inf_ty != ty:
                    self.add_err(TypesMismatchError(
                        f"expected `{ty}`, found `{inf_ty}`"), span)
                else:
                    return ty
            case Binary(kind, left, right):
                lty = self.infer(left)
                rty = self.infer(right)
                if lty == rty:
                    match kind:
                        case BinaryKind.Lt | BinaryKind.Gt:
                            return self.mk_bool()
                    return lty
                else:
                    self.add_err(TypesMismatchError(
                        f"cannot {kind} `{lty}` and `{rty}`"), span)
                    return self.mk_unit()
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
                if not self.scope.search_local(name):
                    self.add_err(NotFound(name, ""), span)
                ty = self.scope.find_local(name)
                expr.ty = ty
                return ty
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
                        if type(expr.kind) != Ident:
                            panic(
                                f"cannot use & on {expr.kind.__class__.__name__}")
                        ty = self.infer(expr)
                        return RefTy(ty)
                    case _:
                        assert False, f"{kind} unreachable"
            case If(cond, body, elze):
                self.check(cond, self.mk_bool())
                expr.kind.cond.ty = self.mk_bool()
                if_ty = self.infer_block(body)
                else_ty = None
                if elze:
                    else_ty = self.infer_block(elze)
                return if_ty
            case Loop(body):
                self.infer_block(body)
                return self.mk_unit()
            case Cast(expr, ty):
                expr.ty = self.infer(expr)
                return ty
            case _:
                assert False, f"{expr.kind}"

    def check(self, expr: Expr, expected_ty: Ty):
        span = expr.span
        match expr.kind:
            case Assign(Ident(name), init):
                if not self.scope.search_local(name):
                    self.add_err(NotFound(name, ""), span)
                ty = self.scope.find_local(name)
                expr.ty = ty
                self.check(init, expected_ty)
            case Binary(kind, left, right):
                match kind:
                    case BinaryKind.Lt | BinaryKind.Gt:
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
                ty = PrimTy(val)
                if expected_ty and ty != expected_ty:
                    self.add_err(TypesMismatchError(
                        f"expected `{expected_ty}`, found `{ty}`"), span)
            case Call(name, args):
                self.check_call(name, args, span, expected_ty)
            case Ident(name):
                if ty := self.scope.find_local(name):
                    if ty != expected_ty:
                        self.add_err(TypesMismatchError(
                            f"expected `{expected_ty}`, found `{ty}`"), span)
                    expr.ty = ty
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
                        if type(expr.kind) != Ident:
                            panic(
                                f"cannot use & on {expr.kind.__class__.__name__}")
                        match expected_ty:
                            case RefTy(ty):
                                self.check(expr, ty)
                            case _:
                                panic(f"expected {expected_ty} but got Raw")
                    case _:
                        assert False, f"{op} unreachable"
            case If(cond, body, elze):
                self.check(cond, self.mk_bool())
                self.check_block(body)
                if elze:
                    self.check_block(elze)
            case Loop(body):
                self.check_block(body)
            case Cast(cast_expr, ty):
                cast_expr.ty = self.infer(cast_expr)
                match cast_expr.ty, ty:
                    case RefTy(_) | PrimTy(PrimTyKind.Raw), PrimTy(PrimTyKind.Raw):
                        expr.ty = ty
                    case PrimTy(_), PrimTy(PrimTyKind.Raw):
                        self.add_err(CastError(
                            f"invalid cast of `{cast_expr.ty}`"), span)
                    case PrimTy(p), RefTy(_) if p != PrimTyKind.Raw:
                        self.add_err(CastError(
                            f"invalid cast of `{cast_expr.ty}`"), span)
                    case PrimTy(_), PrimTy(_):
                        pass
                    case _, _:
                        assert False, "cast"
                if ty != expected_ty:
                    self.add_err(TypesMismatchError(
                        f"expected `{expected_ty}`, found `{ty}`"), span)
            case _:
                assert False

    def check_call(self, name, args, span, expected_ty=None):
        defn = self.defs.get(name)
        if not defn:
            self.add_err(NotFound(name, ""), span)
            return self.mk_unit()

        variadic = False
        starts_at = 0
        for arg in defn.data['args']:
            if type(arg) == Variadic:
                variadic = True
                break
            starts_at += 1

        found = len(args)
        expected = len(defn.data['args'])

        if not variadic and found != expected:
            panic(f"expected {expected} args found {found}")

        if variadic:
            exp = defn.data['args']
            for i, arg in enumerate(args):
                if i >= starts_at:
                    arg.ty = self.infer(arg)
                else:
                    self.check(arg, exp[i].ty)
                    arg.ty = exp[i].ty
        else:
            for arg, exp in zip(args, defn.data['args']):
                self.check(arg, exp.ty)
                arg.ty = exp.ty

        ty = defn.data['ret_ty']
        if expected_ty and ty != expected_ty:
            panic(f"unexpected type: expected {expected_ty} found {ty}")
        return ty

    def check_block(self, block):
        span = block.span
        self.scope.add()
        ret_ty = None
        if self.fn_ctx:
            deff = self.defs[self.fn_ctx]
            assert deff.kind == DefKind.Fn
            fn_dict = deff.data
            for arg in fn_dict['args']:
                if type(arg) == Variadic:
                    continue
                self.scope.def_local(arg.name, arg.ty)
            ret_ty = fn_dict['ret_ty']
        self.fn_ctx = None
        for stmt in block.stmts:
            self.visit_stmt(stmt)
        self.scope.pop()

    def infer_block(self, block):
        span = block.span
        self.scope.add()
        if self.fn_ctx:
            deff = self.defs[self.fn_ctx]
            assert deff.kind == DefKind.Fn
            fn_dict = deff.data
            for arg in fn_dict['args']:
                if type(arg) == Variadic:
                    continue
                self.scope.def_local(arg.name, arg.ty)
        self.fn_ctx = None
        ty = None
        max = len(block.stmts)
        for i, stmt in enumerate(block.stmts):
            self.visit_stmt(stmt)
            if i == max - 1:
                match stmt.kind:
                    case Let():
                        assert False, "expected expression"
                    case Break():
                        pass
                    case _:
                        ty = self.infer(stmt.kind)
        self.scope.pop()
        if ty:
            return ty

    def visit_stmt(self, stmt: Stmt):
        span = stmt.span
        match stmt.kind:
            case Let(name, ty, init):
                if self.scope.search_local(name):
                    self.add_err(Redefinition(f"{name} is not defined"), span)
                if ty:
                    self.check(init, ty)
                else:
                    ty = self.infer(init)
                    stmt.kind.ty = ty
                self.scope.def_local(name, ty)
            case Break():
                pass
            case _:
                self.infer(stmt.kind)

    def visit_block(self, block: Block):
        self.scope.add()
        if self.fn_ctx:
            deff = self.defs[self.fn_ctx]
            assert deff.kind == DefKind.Fn
            fn_dict = deff.data
            for arg in fn_dict['args']:
                if type(arg) == Variadic:
                    continue
                self.scope.def_local(arg.name, arg.ty)
        self.fn_ctx = None
        for stmt in block.stmts:
            self.visit_stmt(stmt)
        self.scope.pop()

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
            match node:
                case Fn():
                    self.visit_fn(node)
                case _:
                    assert False, f"{node}"
