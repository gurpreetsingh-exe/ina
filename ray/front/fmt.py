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
            case Assign(Ident(name), init):
                return "{} = {}".format(name, self.expr(init))
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
            case Loop(body):
                return "loop {}".format(self.block(body))
            case _:
                assert False, f"{expr} not implemented"

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
                return "{}{}".format(self.expr(stmt.kind), ";" if stmt.semi else "")

    def fn(self, fn: Fn) -> str:
        args = []
        for arg in fn.args:
            match arg:
                case Arg(name, ty):
                    args.append("{}: {}".format(name, ty))
                case Variadic():
                    args.append("...")
        args = ", ".join(args)
        ret_ty = " -> {}".format(fn.ret_ty) if fn.ret_ty != PrimTy(
            PrimTyKind.Unit) else ""
        if not fn.body:
            return "{}fn {}({}){}".format(
                "extern " if fn.is_extern else "",
                fn.name,
                args,
                ret_ty)
        else:
            return "{}fn {}({}){} {}".format(
                "extern " if fn.is_extern else "",
                fn.name,
                args,
                ret_ty,
                self.block(fn.body))

    def render(self, mod: Module):
        for item in mod:
            match item:
                case Fn():
                    return self.fn(item)
