from .ray import *


class Node:
    pass


class Func(Node):
    __match_args__ = ("name", "args", "ret_ty", "body", "is_extern")
    def __init__(self, name, args, ret_ty, body, is_extern):
        self.name = name
        self.args = args
        self.ret_ty = ret_ty
        self.body = body
        self.is_extern = is_extern

class BBlock(Node):
    def __init__(self, nodes):
        self.nodes = nodes

class Alloc(Node):
    def __init__(self, ty):
        self.ty = ty

class Store(Node):
    def __init__(self, node, value):
        self.node = node
        self.value = value

class BinOp(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

class Add(BinOp):
    def __init__(self, left, right):
        super().__init__(left, right)

class Sub(BinOp):
    def __init__(self, left, right):
        super().__init__(left, right)

class Mul(BinOp):
    def __init__(self, left, right):
        super().__init__(left, right)

class Div(BinOp):
    def __init__(self, left, right):
        super().__init__(left, right)

class LiteralNode(Node):
    def __init__(self, val):
        self.val = val

class Int(LiteralNode):
    def __init__(self, val):
        super().__init__(val)

class Float(LiteralNode):
    def __init__(self, val):
        super().__init__(val)

class Str(LiteralNode):
    def __init__(self, val):
        super().__init__(val)

class Bool(LiteralNode):
    def __init__(self, val):
        super().__init__(val)

class Char(LiteralNode):
    def __init__(self, val):
        super().__init__(val)


class IRGen:
    def __init__(self, typed_ast):
        self.typed_ast = typed_ast
        self.ir = self.gen(self.typed_ast)

    def expr(self, expr):
        ir = []
        match expr:
            case Binary(op, left, right):
                lhs = self.expr(left)
                ir.append(lhs)
                l = lhs.id

                rhs = self.expr(right)
                ir.append(rhs)
                r = rhs.id

                match op.kind:
                    case TokenKind.PLUS:
                        return Add(l, r)
                    case TokenKind.MINUS:
                        return Sub(l, r)
                    case TokenKind.STAR:
                        return Mul(l, r)
                    case TokenKind.SLASH:
                        return Div(l, r)
                    case _:
                        panic(f"{op}")
            case Literal(kind, value):
                val = None
                match kind:
                    case Lit.Int:
                        val = Int(value)
                    case Lit.Float:
                        val = Float(value)
                    case Lit.Bool:
                        val = Bool(value)
                    case Lit.Str:
                        val = Str(value)
                    case Lit.Char:
                        val = Char(value)
                    case _:
                        panic(f"unexpected literal {kind}")
                ir.append(val)
        return ir

    def gen(self, nodes):
        ir = []
        for node in nodes:
            match node:
                case Fn(name, args, ret_ty, body, is_extern):
                    body = self.gen(body.stmts)
                    ir.append(Func(name, args, ret_ty, body, is_extern))
                case Let(name, ty, init):
                    alloc = Alloc(ty.ty)
                    ir.append(alloc)
                    expr = self.expr(init)
                    ir.append(expr)
                    ir.append(Store(alloc.id, expr.id))
                case _:
                    panic(f"{node} is not implemented")
        return ir
