#!/usr/bin/python

# TODO: refactor primitive types and create
#       i8..i64 ints and f16..f64 floats.
#       before that refactor the Ast nodes

import sys
from beeprint import pp

from Lexer import *
from Ast import *
from utils import *
from Parser import Parser
from tychk import TyCheck

regs = ["rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
fn_regs = [
    ["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
    ["xmm0", "xmm1"]
]

class Scope:
    def __init__(self):
        self.scopes = []
        self.stack_offset = 0

    def search_local(self, name):
        for scope in reversed(self.scopes):
            for var in scope:
                if var['name'] == name:
                    return True
        return False

    def def_local(self, name, ty):
        if self.search_local(name):
            panic(f"{name} is already defined")
        size = 0
        size = ty.get_size()

        self.stack_offset += size
        self.scopes[-1].append({ 'name': name, 'off': self.stack_offset })
        return self.stack_offset

    def find_local(self, name):
        for scope in reversed(self.scopes):
            for var in scope:
                if var['name'] == name:
                    return var['off']

    def add(self):
        self.scopes.append([])

    def pop(self):
        self.scopes.pop()
        self.stack_offset = 0

class Codegen:
    def __init__(self, ast, defs):
        self.ast = ast
        self.buf = ""
        self.scopes = Scope()
        self.strings = []
        self.floats = []
        self.defs = defs
        self.label_id = 1
        self.fn_ctx: Fn | None = None
        self.debug_messages = True

    @property
    def label(self):
        i = self.label_id
        self.label_id += 1
        return i

    def dbg(self, msg: str):
        if self.debug_messages:
            self.buf += f"{msg}\n"

    def append(self, expr: Expr, stack: List[Expr]):
        match expr:
            case Binary(_, left, _):
                stack += self.binop_stack(left.kind)
            case _:
                stack.append(expr)

    def binop_stack(self, binary: Binary) -> List[Expr]:
        stack: List[Expr] = []
        match binary:
            case Binary(_):
                self.append(binary.left, stack)
                self.append(binary.right, stack)
                stack.append(binary.op)
            case _:
                stack.append(binary)
        return stack

    def expr(self, expr: Expr, reg, label = 0):
        match expr.kind:
            case Binary():
                self.dbg(f"    # binary")
                stack = self.binop_stack(expr.kind)
                stack0 = []
                for i, expr in enumerate(stack):
                    match expr:
                        case Token(kind, _):
                            b = stack0.pop()
                            a = stack0.pop()
                            match kind:
                                case TokenKind.PLUS:
                                    self.buf += f"    add {a}, {b}\n"
                                case TokenKind.MINUS:
                                    self.buf += f"    sub {a}, {b}\n"
                                case TokenKind.LT | TokenKind.GT:
                                    self.buf += f"    cmp {a}, {b}\n"
                                    if reg:
                                        setT = "setl" if kind == TokenKind.LT else "setg"
                                        self.buf += f"    {setT} al\n"
                                        self.buf += f"    movzx {a}, al\n"
                                    else:
                                        if label:
                                            jmp = "jge" if kind == TokenKind.LT else "jle"
                                            self.buf += f"    {jmp} .L{label}\n"
                                case _:
                                    assert False, f"{kind} not implemented"
                            stack0.append(a)
                        case _:
                            self.expr(expr, regs[i])
                            stack0.append(regs[i])
                if reg and reg != "rax":
                    self.buf += f"    mov {reg}, rax\n"
            case Ident(name):
                off = self.scopes.find_local(name)
                self.dbg(f"    # load {name}")
                if reg:
                    if reg in fn_regs[1]:
                        self.buf += f"    mov rax, [rbp - {off}]\n"
                        self.buf += f"    movq {reg}, rax\n"
                    else:
                        self.buf += f"    mov {reg}, [rbp - {off}]\n"
                else:
                    self.buf += f"    mov rax, [rbp - {off}]\n"
            case Call(name, args):
                self.dbg(f"    # function call {name}")
                defn = self.defs.get(name)
                variadic = False
                for arg in defn.data['args']:
                    if type(arg) == Variadic:
                        variadic = True
                        break

                i = 0
                for arg in args:
                    if arg.ty.ty == TyKind.Float:
                        continue
                    self.expr(arg, fn_regs[0][i])
                    i += 1

                flt_args = 0
                i = 0
                for arg in args:
                    if is_float := arg.ty.ty == TyKind.Float:
                        flt_args += is_float
                        self.expr(arg, fn_regs[1][i])
                        i += 1

                if defn.data['abi']:
                    match abi := defn.data['abi'][1:-1], variadic:
                        case "C", True:
                            if flt_args:
                                self.buf += f"    mov rax, {flt_args}\n"
                            else:
                                self.buf += "    xor rax, rax\n"
                        case "C", False:
                            pass
                        case _:
                            assert False, f"{abi} abi is not implemented"
                self.buf += f"    call {name}\n"
                if defn.data['ret_ty'] and reg and reg != "rax":
                    self.dbg(f"    # return")
                    self.buf += f"    mov {reg}, rax\n"
            case Literal(kind, value):
                if not reg:
                    return
                self.dbg(f"    # {kind}: {value}")
                match kind:
                    case Lit.Str:
                        self.buf += f"    mov {reg}, OFFSET FLAT:str{len(self.strings)}\n"
                        self.strings.append(value)
                    case Lit.Bool:
                        self.buf += f"    mov {reg}, {1 if value == 'true' else 0}\n"
                    case Lit.Float:
                        label = self.label
                        self.buf += f"    movsd {reg}, QWORD PTR .L{label}[rip]\n"
                        self.floats.append((label, value))
                    case _:
                        self.buf += f"    mov {reg}, {value}\n"
            case Unary(op, expr):
                self.dbg(f"    # unary {get_token_name(op.kind)}")
                if not reg:
                    match op.kind:
                        case TokenKind.AMPERSAND:
                            assert False
                        case TokenKind.BANG:
                            self.buf += f"    mov rbx, 1\n"
                            self.expr(expr, None)
                            self.buf += f"    sub rbx, rax\n"
                            self.buf += f"    mov rax, rbx\n"
                        case TokenKind.MINUS:
                            self.buf += f"    mov rbx, 0\n"
                            self.expr(expr, None)
                            self.buf += f"    sub rbx, rax\n"
                            self.buf += f"    mov rax, rbx\n"
                        case _:
                            assert False, "not implemented"
                    return
                match op.kind:
                    case TokenKind.AMPERSAND:
                        assert type(expr.kind) == Ident
                        off = self.scopes.find_local(expr.kind.name)
                        self.buf += f"    lea {reg}, [rbp - {off}]\n"
                    case TokenKind.BANG:
                        self.expr(expr, reg)
                        self.buf += f"    xor {reg}, 1\n"
                    case TokenKind.MINUS:
                        self.expr(expr, reg)
                        self.buf += f"    neg {reg}\n"
                    case _:
                        assert False, "not implemented"
            case If(cond, body, elze):
                self.dbg(f"    # if-else")
                has_else = bool(elze)
                label_false = self.label
                label_end = None
                if has_else:
                    label_end = self.label
                self.expr(cond, None, label_false)
                match cond.kind:
                    case Literal():
                        if cond.kind.value == 'false':
                            self.buf += f"    jmp .L{label_false}\n"
                    case Ident() | Unary() | Call() | Binary():
                        self.buf += f"    cmp rax, 0\n"
                        self.buf += f"    je .L{label_false}\n"
                    case _:
                        assert False, f"{cond.kind}"
                self.dbg(f"    # if body")
                self.gen_block(body)
                if has_else:
                    self.buf += f"    jmp .L{label_end}\n"
                self.buf += f".L{label_false}:\n"
                if has_else:
                    self.dbg(f"    # else body")
                    self.gen_block(elze)
                    self.buf += f".L{label_end}:\n"
                if reg and reg != "rax":
                    self.buf += f"    mov {reg}, rax\n"
            case Cast(expr, ty):
                self.dbg(f"    # cast {ty}")
                if expr.ty == ty:
                    self.expr(expr, reg)
                    return
                match ty.ty:
                    case TyKind.Float:
                        self.expr(expr, reg)
                        self.buf += f"    cvtsi2sd xmm0, {reg}\n"
                        self.buf += f"    movq {reg}, xmm0\n"
                    case TyKind.Int:
                        self.expr(expr, "xmm0")
                        self.buf += f"    cvttsd2si {reg}, xmm0\n"
                    case _:
                        assert False
            case _:
                assert False, f"{expr} is not implemented"

    def stmt(self, stmt: Stmt):
        match stmt.kind:
            case Let(name, ty, init):
                off = self.scopes.def_local(name, ty)
                self.dbg(f"    # let {name}: {ty}")
                if type(init) == Literal:
                    match init.kind:
                        case Lit.Str:
                            self.expr(init, "rax")
                            self.buf += f"    mov [rbp - {off}], rax\n"
                        case Lit.Int:
                            self.buf += f"    mov QWORD PTR [rbp - {off}], {init.value}\n"
                        case Lit.Bool:
                            self.buf += f"    mov QWORD PTR [rbp - {off}], {1 if init.value == 'true' else 0}\n"
                        case Lit.Float:
                            self.expr(init, "xmm0")
                            self.buf += f"    movsd [rbp - {off}], xmm0\n"
                        case _:
                            assert False
                else:
                    self.expr(init, "rax")
                    self.buf += f"    mov [rbp - {off}], rax\n"
            case _:
                self.expr(stmt.kind, "rax")

    def gen_block(self, block: Block):
        self.scopes.add()
        if self.fn_ctx:
            args = self.fn_ctx.args
            for i, arg in enumerate(args):
                match arg:
                    case Arg(name, ty):
                        off = self.scopes.def_local(name, ty)
                        is_float = arg.ty.ty == TyKind.Float
                        self.dbg(f"    # {name}: {ty}")
                        self.buf += f"    mov [rbp - {off}], {fn_regs[is_float][i]}\n"
                    case Variadic():
                        continue
                    case _:
                        assert False, "unreachable"
        self.fn_ctx = None
        for stmt in block.stmts:
            self.stmt(stmt)
        self.scopes.pop()

    def gen(self, nodes):
        for node in nodes:
            match node:
                case Fn(name, _, ret_ty, body, is_extern, _):
                    if is_extern:
                        continue
                    self.dbg(f"# function {name}")
                    self.buf += f"{name}:\n    push rbp\n    mov rbp, rsp\n"
                    alignment = node.stack_alignment
                    if alignment:
                        self.dbg("    # stack alignment")
                        self.buf += f"    sub rsp, {alignment}\n"
                    assert body != None
                    self.fn_ctx = node
                    self.gen_block(body)
                    if alignment:
                        self.dbg("    # stack alignment")
                        self.buf += f"    add rsp, {alignment}\n"
                    if not ret_ty:
                        self.dbg(f"    # return {name}")
                        self.buf += f"    xor rax, rax\n"
                    self.buf += "    pop rbp\n    ret\n"
                case _:
                    assert False, f"{node} is not implemented"

    def emit_data(self):
        import struct
        self.buf += ".data\n"
        for i, string in enumerate(self.strings):
            self.buf += f"str{i}:\n"
            self.buf += f"    .string {string}\n"
        for i, flt in self.floats:
            self.buf += f".L{i}:\n"
            flt = struct.pack('d', float(flt))
            hi = int.from_bytes(flt[:4], 'little')
            lo = int.from_bytes(flt[4:], 'little')
            self.buf += f"    .long {hi}\n    .long {lo}\n"

    def emit(self):
        self.buf += ".intel_syntax noprefix\n.text\n.globl main\n"
        self.gen(self.ast)
        self.emit_data()
        return self.buf


def usage(arg0):
    print(f"Usage: {arg0} [command] [options] input...")
    print("\nCommands:")
    print("    com          compile the file")
    print("\nOptions:")
    print("    -h, --help   print help information\n")
    exit(1)

class Command(Enum):
    Nan = auto()
    Compile = auto()

def main(argv):
    filename = ""
    arg0 = argv[0]
    argv = argv[1:]
    command = Command.Nan
    while argv:
        arg = argv[0]
        if arg.startswith("-"):
            if len(arg) < 2:
                usage(arg0)
            match arg[1]:
                case '-':
                    match arg:
                        case "--help":
                            usage(arg0)
                        case _:
                            print(f"Unknown option \"{arg}\"")
                            usage(arg0)
                case 'h':
                    usage(arg0)
                case _:
                    print(f"Unknown option \"{arg}\"")
                    usage(arg0)
        elif command == Command.Nan:
            match arg:
                case "com":
                    command = Command.Compile
                case _:
                    print(f"Unknown command \"{arg}\"")
        else:
            match command:
                case Command.Nan:
                    usage(arg0)
                case _:
                    if not filename:
                        filename = arg
                    else:
                        usage(arg0)
        argv = argv[1:]

    match command:
        case Command.Nan:
            usage(arg0)
        case _:
            if not filename:
                usage(arg0)
            match command:
                case Command.Compile:
                    lexer = lexer_from_file(filename)
                    src = lexer.program
                    tokens = list(lexer.lexfile())
                    parser = Parser(src, tokens)
                    ast = list(parser.parse())
                    tychk = TyCheck(ast)
                    # gen = IRGen(ast)
                    # pp(ast, max_depth=10)
                    code = Codegen(ast, tychk.defs).emit()
                    output = filename.split('.')[0]
                    with open(f"{output}.asm", "w") as f:
                        f.write(code)

                    from subprocess import call
                    call(["as", f"{output}.asm", "-o", f"{output}.o"])
                    call(["gcc", f"{output}.o", "-o", output])
                case _:
                    assert False, "unreachable"


if __name__ == "__main__":
    main(sys.argv)
