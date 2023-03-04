from ..Ast import *

cast = {
    Lit.Int: int,
    Lit.Float: float,
}


class ConstantFolder:
    def __init__(self, ast) -> None:
        self.ast = ast

    def fold_expr(self, expr) -> Expr:
        match expr.kind:
            case Assign(_, init):
                init = self.fold_expr(init)
            case Binary(kind, left, right):
                left = self.fold_expr(left)
                right = self.fold_expr(right)
                match left.kind, right.kind:
                    case Literal(lkind, lvalue), Literal(rkind, rvalue):
                        assert lkind == rkind
                        if lkind in {Lit.Int, Lit.Float}:
                            cast_ty = cast[lkind]
                            match kind:
                                case BinaryKind.Add:
                                    expr.kind = Literal(lkind, str(
                                        cast_ty(lvalue) + cast_ty(rvalue)))
                                case BinaryKind.Sub:
                                    expr.kind = Literal(lkind, str(
                                        cast_ty(lvalue) - cast_ty(rvalue)))
                                case BinaryKind.Mul:
                                    expr.kind = Literal(lkind, str(
                                        cast_ty(lvalue) * cast_ty(rvalue)))
                                case BinaryKind.Div:
                                    expr.kind = Literal(lkind, str(
                                        cast_ty(lvalue) / cast_ty(rvalue)))
                                case BinaryKind.Mod:
                                    expr.kind = Literal(lkind, str(
                                        cast_ty(lvalue) % cast_ty(rvalue)))
                                case BinaryKind.Lt:
                                    expr.kind = Literal(Lit.Bool, str(
                                        cast_ty(lvalue) < cast_ty(rvalue)))
                                case BinaryKind.Gt:
                                    expr.kind = Literal(Lit.Bool, str(
                                        cast_ty(lvalue) > cast_ty(rvalue)))
                                case BinaryKind.Eq:
                                    expr.kind = Literal(Lit.Bool, str(
                                        cast_ty(lvalue) == cast_ty(rvalue)))
                                case BinaryKind.NotEq:
                                    expr.kind = Literal(Lit.Bool, str(
                                        cast_ty(lvalue) != cast_ty(rvalue)))
                                case _:
                                    assert False
                    case Literal(), _:
                        comp = [BinaryKind.Lt, BinaryKind.Gt]
                        if kind in comp:
                            match kind:
                                case BinaryKind.Lt:
                                    expr.kind.kind = BinaryKind.Gt
                                case BinaryKind.Gt:
                                    expr.kind.kind = BinaryKind.Lt
                            expr.kind.left = right
                            expr.kind.right = left
                    case _, Binary():
                        expr.kind.left = right
                        expr.kind.right = left
                    case _, _:
                        pass
            case Call(_, args):
                for arg in args:
                    arg = self.fold_expr(arg)
            case If(cond, body, elze):
                cond = self.fold_expr(cond)
                self.visit_block(body)
                if elze:
                    self.visit_block(elze)
            case Loop(body):
                self.visit_block(body)
        return expr

    def visit_block(self, block):
        for stmt in block.stmts:
            match stmt.kind:
                case Let(_, _, init):
                    stmt.kind.init = self.fold_expr(init)
                case Expr(_):
                    self.fold_expr(stmt.kind)
        if block.expr:
            self.fold_expr(block.expr)

    def fold(self):
        for node in self.ast:
            match node:
                case Fn(_, _, _, body, _, _):
                    self.visit_block(body)
                case Const(name, ty, init):
                    pass
                case ExternBlock(_):
                    pass
