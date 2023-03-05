from ..Ast import *


class AstRenderer:
    def __init__(self):
        self.indent_level = 0
        self.width = 4

    @property
    def indent(self) -> str:
        return " " * self.indent_level * self.width

    def expr(self, expr: Expr) -> str:
        match expr:
            case Binary(kind, left, right):
                return "{} {} {}".format(self.expr(left),
                                         kind.to_display(), self.expr(right))
            case Literal(_, value):
                return value
            case Call(name, args):
                return "{}({})".format(name, ", ".join(map(lambda a: self.expr(a), args)))
            case Unary(kind, expr):
                return "{}{}".format(kind.to_display(), self.expr(expr))
            case Ident(name):
                return name
            case If(cond, then, elze):
                if elze:
                    return "if {} {} else {}".format(self.expr(cond), self.block(then), self.block(elze))
                else:
                    return "if {} {}".format(self.expr(cond), self.block(then))
            case _:
                assert False, "not implemented"

    def block(self, block: Block) -> str:
        self.indent_level += 1
        padding = self.indent
        if not (block.stmts or block.expr):
            self.indent_level -= 1
            return "{}"
        res = "".join(map(lambda s: "{}{}\n".format(
            padding, self.stmt(s)), block.stmts))
        if block.expr:
            res += "{}{}\n".format(padding, self.expr(block.expr))
        self.indent_level -= 1
        padding = self.indent
        return "{{\n{}{}}}".format(res, padding)

    def stmt(self, stmt: Stmt) -> str:
        match stmt.kind:
            case Let(name, ty, init):
                return "let {}{} = {};".format(name, ": {}".format(ty) if ty else "", self.expr(init))
            case Break():
                return "break;"
            case _:
                return "{};".format(self.expr(stmt.kind))
