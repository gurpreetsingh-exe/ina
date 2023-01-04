from __future__ import annotations
from Ast import *
from Token import *
from utils import panic

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

    def search_local(self, name):
        for scope in reversed(self.scopes):
            for var in scope:
                if var['name'] == name:
                    return True
        return False

    def find_local(self, name):
        for scope in reversed(self.scopes):
            for var in scope:
                if var['name'] == name:
                    return var['ty']

    def def_local(self, name, ty, val=None):
        if self.search_local(name):
            panic(f"{name} is already defined")
        self.scopes[-1].append({ 'name': name, 'ty': ty, 'val': val })

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
        self.tychk()

    def infer(self, expr: Expr) -> Ty:
        match expr.kind:
            case Binary(op, left, right):
                lty = self.infer(left)
                rty = self.infer(right)
                if lty == rty:
                    match op:
                        case Token(TokenKind.LT | TokenKind.GT, _):
                            return Ty(TyKind.Bool)
                    return lty
                else:
                    panic(f"{op} is not implemented for {lty} and {rty}")
            case Literal(kind, _):
                val = None
                match kind:
                    case Lit.Int:
                        val = TyKind.Int
                    case Lit.Float:
                        val = TyKind.Float
                    case Lit.Bool:
                        val = TyKind.Bool
                    case Lit.Str:
                        val = TyKind.Str
                    case Lit.Char:
                        val = TyKind.Char
                    case _:
                        panic(f"unexpected literal {kind}")
                ty = Ty(val)
                expr.ty = ty
                return ty
            case Call(name, args):
                if ty := self.check_call(name, args):
                    return ty
                else:
                    panic("return type expected")
            case Ident(name):
                if not self.scope.search_local(name):
                    panic(f"{name} is not defined")
                ty = self.scope.find_local(name)
                expr.ty = ty
                return ty
            case Unary(op, expr):
                match op.kind:
                    case TokenKind.BANG:
                        if (ty := self.infer(expr).ty) != TyKind.Bool:
                            panic(f"expected bool but got {ty}")
                        return Ty(TyKind.Bool)
                    case TokenKind.MINUS:
                        if (ty := self.infer(expr).ty) not in [TyKind.Int, TyKind.Float]:
                            panic(f"expected number but got {ty}")
                        return Ty(ty)
                    case TokenKind.AMPERSAND:
                        if type(expr.kind) != Ident:
                            panic(f"cannot use & on {expr.kind.__class__.__name__}")
                        return Ty(TyKind.Raw)
                    case _:
                        assert False, f"{op} unreachable"
            case If(cond, body, elze):
                self.check(cond, Ty(TyKind.Bool))
                if_ty = self.infer_block(body)
                else_ty = None
                if elze:
                    else_ty = self.infer_block(elze)
                return if_ty
            case Cast(expr, ty):
                expr.ty = self.infer(expr)
                return ty
            case _:
                assert False

    def check(self, expr: Expr, expected_ty: Ty):
        match expr.kind:
            case Binary(op, left, right):
                match op:
                    case Token(TokenKind.LT | TokenKind.GT, _):
                        if expected_ty.ty != TyKind.Bool:
                            panic(f"expected {expected_ty.ty} but got bool")
                        lty = self.infer(left)
                        rty = self.infer(right)
                    case _:
                        self.check(left, expected_ty)
                        self.check(right, expected_ty)
            case Literal(kind, _):
                val = None
                match kind:
                    case Lit.Int:
                        val = TyKind.Int
                    case Lit.Float:
                        val = TyKind.Float
                    case Lit.Bool:
                        val = TyKind.Bool
                    case Lit.Str:
                        val = TyKind.Str
                    case Lit.Char:
                        val = TyKind.Char
                    case _:
                        panic(f"unexpected literal {kind}")
                ty = Ty(val)
                if expected_ty and ty != expected_ty:
                    panic(f"expected {expected_ty} but got {ty}")
            case Call(name, args):
                self.check_call(name, args, expected_ty)
            case Ident(name):
                if ty := self.scope.find_local(name):
                    if ty != expected_ty:
                        panic(f"expected {expected_ty}, found {ty}")
                else:
                    panic(f"{name} is not defined")
            case Unary(op, expr):
                match op.kind:
                    case TokenKind.BANG:
                        if expected_ty.ty != TyKind.Bool:
                            panic(f"expected {expected_ty} but got Bool")
                        self.check(expr, expected_ty)
                    case TokenKind.MINUS:
                        if expected_ty.ty not in [TyKind.Int, TyKind.Float]:
                            panic(f"expected {expected_ty} but got number")
                        self.check(expr, expected_ty)
                    case TokenKind.AMPERSAND:
                        if type(expr.kind) != Ident:
                            panic(f"cannot use & on {expr.kind.__class__.__name__}")
                        if expected_ty.ty != TyKind.Raw:
                            panic(f"expected {expected_ty} but got Raw")
                    case _:
                        assert False, f"{op} unreachable"
            case If(cond, body, elze):
                self.check(cond, Ty(TyKind.Bool))
                self.check_block(body)
                if elze:
                    self.check_block(elze)
            case Cast(expr, ty):
                expr.ty = self.infer(expr)
                return ty
            case _:
                assert False

    def check_call(self, name, args, expected_ty=None):
        defn = self.defs.get(name)
        if not defn:
            panic(f"{name} is not defined")

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

    def check_stmt(self, stmt: Stmt, expected_ty: Ty):
        match stmt.kind:
            case Let(name, ty, init):
                if not ty:
                    ty = self.infer(init)
                    init.ty = ty
                else:
                    self.check(init, ty)
                    init.ty = ty
                stmt.kind.ty = ty
                self.scope.def_local(name, ty)
            case Expr():
                self.check(stmt.kind, expected_ty)
                stmt.kind.ty = expected_ty
                return expected_ty
            case _:
                assert False

    def infer_stmt(self, stmt):
        match stmt:
            case Let(name, ty, init):
                if not ty:
                    ty = self.infer(init)
                    stmt.ty = ty
                    init.ty = ty
                else:
                    self.check(init, ty)
                    init.ty = ty
                self.scope.def_local(name, ty)
            case _:
                ty = self.infer(stmt)
                stmt.ty = ty
                return ty

    def check_block(self, block):
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
            self.check_stmt(stmt, ret_ty)
        self.scope.pop()

    def infer_block(self, block):
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
        tys = []
        for stmt in block.stmts:
            tys.append(self.infer_stmt(stmt))
        self.scope.pop()
        if tys:
            return tys[-1]

    def tychk(self):
        for node in self.ast:
            match node:
                case Fn(name, args, ret_ty, body, _, abi):
                    fn_dict = { 'args': args, 'ret_ty': ret_ty, 'abi': abi }
                    self.defs[name] = Def(DefKind.Fn, fn_dict)
                    self.fn_ctx = name
                    if body:
                        self.check_block(body)
                case _:
                    assert False, f"{node}"


