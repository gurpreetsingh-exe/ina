from __future__ import annotations
from typing import Any, Dict, Tuple
from beeprint import pp

from ..front.Lexer import *
from ..Ast import *
from ..utils import *
from ..front.Parser import Parser
from ..sema.tychk import TyCheck
from ..codegen.x86_64 import Gen
from ..builtin.intrinsics import builtins_
from ..optimize.constant_fold import ConstantFolder
from ..ast_lowering.IrGen import *
from ..ir.cfg_dump import dump_cfg
from ..ast_lowering.dominance_frontier import dominance_frontier

regs = ["rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8",
        "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
fn_regs = [
    ["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
    ["xmm0", "xmm1"]
]


def align(n, a):
    return ((n + a - 1) // a) * a


def cast_i64_f64(self, expr, reg):
    self.expr(expr, "rax")
    self.buf += f"    cvtsi2sd {reg}, rax\n"


def cast_i64_str(self, expr, reg):
    self.expr(expr, reg)


def cast_f64_i64(self, expr, reg):
    self.expr(expr, "xmm0")
    self.buf += f"    cvttsd2si {reg}, xmm0\n"


def cast_raw_i64(self, expr, reg):
    self.expr(expr, reg)


cast_map = {
    PrimTyKind.I64: {
        PrimTyKind.F64: cast_i64_f64,
        PrimTyKind.Str: cast_i64_str,
    },
    PrimTyKind.F64: {
        PrimTyKind.I64: cast_f64_i64,
    },
    PrimTyKind.Raw: {
        PrimTyKind.I64: cast_raw_i64,
    }
}


reg_ = {
    "rax": {
        1: "al",
        2: "ax",
        4: "eax",
        8: "rax",
    },
    "rbx": {
        1: "bl",
        2: "bx",
        4: "ebx",
        8: "rbx",
    },
    "rcx": {
        1: "cl",
        2: "cx",
        4: "ecx",
        8: "rcx",
    },
    "rdx": {
        1: "dl",
        2: "dx",
        4: "edx",
        8: "rdx",
    },
    "rdi": {
        1: "dil",
        2: "di",
        4: "edi",
        8: "rdi",
    },
    "rsi": {
        1: "sil",
        2: "si",
        4: "esi",
        8: "rsi",
    },
    "xmm0": {
        1: "xmm0",
        2: "xmm0",
        4: "xmm0",
        8: "xmm0",
    },
    "xmm1": {
        1: "xmm1",
        2: "xmm1",
        4: "xmm1",
        8: "xmm1",
    }
}

for reg in regs[6:]:
    reg_[reg] = {
        1: f"{reg}b",
        2: f"{reg}w",
        4: f"{reg}d",
        8: reg,
    }


class Env:
    def __init__(self, parent: Env | None = None):
        self.parent = parent
        self.bindings = []
        self.stack_offset = self.parent.stack_offset if self.parent else 0

    def search_local(self, name):
        found = False
        for var in self.bindings:
            if var['name'] == name:
                found = True
        if found:
            return True
        if not self.parent:
            return False
        return self.parent.search_local(name)

    def def_local(self, name, ty) -> Tuple[int, int]:
        if self.search_local(name):
            panic(f"{name} is already defined")

        size = ty.get_size()
        self.stack_offset = (self.stack_offset + size * 2 - 1) & ~(size - 1)
        self.bindings.append({'name': name,
                              'off': self.stack_offset, 'size': size})
        return self.stack_offset, size

    def find_local(self, name) -> Dict[str, Any]:
        for var in self.bindings:
            if var['name'] == name:
                return var
        if not self.parent:
            assert False
        return self.parent.find_local(name)


class Codegen:
    def __init__(self, ast, defs):
        self.ast = ast
        self.buf = ""
        self.env: Env = Env()
        self.strings = []
        self.floats = []
        self.defs = defs
        self.label_id = 1
        self.breaks = []
        self.fn_ctx: Fn | None = None
        self.debug_messages = 0
        self.consts = {}
        self.types = {}

    @property
    def label(self):
        i = self.label_id
        self.label_id += 1
        return i

    def dbg(self, msg: str):
        if self.debug_messages:
            self.buf += f"{msg}\n"

    def append(self, expr: Expr, stack: List[Expr | BinaryKind]):
        match expr.kind:
            case Binary():
                stack += self.binop_stack(expr.kind)
            case _:
                stack.append(expr)

    def binop_stack(self, binary: Binary) -> List[Expr | BinaryKind]:
        stack: List[Expr | BinaryKind] = []
        match binary:
            case Binary(kind, left, right):
                self.append(left, stack)
                self.append(right, stack)
                stack.append(kind)
            case _:
                stack.append(binary)
        return stack

    def reg_from_sz(self, reg: str, sz: int) -> Tuple[str, str]:
        ptr = ""
        match sz:
            case 1:
                ptr = "BYTE"
            case 2:
                ptr = "WORD"
            case 4:
                ptr = "DWORD"
            case 8:
                ptr = "QWORD"
        if sz != 8:
            reg = reg_[reg][sz]
        return f"{ptr} PTR", reg

    def load_var(self, name, reg):
        var = self.env.find_local(name)
        off = var['off']
        sz = var['size']
        ptr, __reg = self.reg_from_sz(reg, sz)
        self.dbg(f"    # load {name}")
        if reg:
            if reg in fn_regs[0] + regs:
                if sz in [1, 2]:
                    self.buf += f"    movzx {reg}, {ptr} [rbp - {off}]\n"
                else:
                    self.buf += f"    mov {__reg}, {ptr} [rbp - {off}]\n"
            else:
                if sz == 8:
                    self.buf += f"    movsd {reg}, [rbp - {off}]\n"
                elif sz == 4:
                    self.buf += f"    movss {reg}, [rbp - {off}]\n"
                    self.buf += f"    cvtss2sd xmm0, xmm0\n"
        else:
            if sz in [1, 2]:
                self.buf += f"    movzx al, {ptr} [rbp - {off}]\n"
            else:
                self.buf += f"    mov rax, {ptr} [rbp - {off}]\n"

    def expr(self, expr: Expr, reg, label=0):
        match expr.kind:
            case Assign(Ident(name), expr):
                self.dbg(f"    # assign")
                var = self.env.find_local(name)
                off = var["off"]
                if reg:
                    self.expr(expr, reg)
                    self.buf += f"    mov [rbp - {off}], {reg}\n"
                else:
                    self.expr(expr, "rax")
                    self.buf += f"    mov [rbp - {off}], rax\n"
            case Binary():
                self.dbg(f"    # binary")
                stack = self.binop_stack(expr.kind)
                stack0 = []
                for i, _expr in enumerate(stack):
                    match _expr:
                        case BinaryKind():
                            kind = _expr
                            b = stack0.pop()
                            a = stack0.pop()
                            match kind:
                                case BinaryKind.Add:
                                    self.buf += f"    add {a}, {b}\n"
                                case BinaryKind.Sub:
                                    self.buf += f"    sub {a}, {b}\n"
                                case BinaryKind.Mul:
                                    self.buf += f"    imul {a}, {b}\n"
                                case BinaryKind.Lt | BinaryKind.Gt:
                                    self.buf += f"    cmp {a}, {b}\n"
                                    if reg:
                                        setT = "setl" if kind == BinaryKind.Lt else "setg"
                                        self.buf += f"    {setT} al\n"
                                        self.buf += f"    movzx {a}, al\n"
                                    else:
                                        if label:
                                            jmp = "jge" if kind == BinaryKind.Lt else "jle"
                                            self.buf += f"    {jmp} .L{label}\n"
                                        else:
                                            assert False
                                case BinaryKind.Eq | BinaryKind.NotEq:
                                    self.buf += f"    cmp {a}, {b}\n"
                                    if reg:
                                        setT = "sete" if kind == BinaryKind.Eq else "setne"
                                        self.buf += f"    {setT} al\n    and al, 1\n"
                                        self.buf += f"    movzx {a}, al\n"
                                    else:
                                        if label:
                                            jmp = "jne" if kind == BinaryKind.Eq else "je"
                                            self.buf += f"    {jmp} .L{label}\n"
                                        else:
                                            assert False
                                case BinaryKind.Mod:
                                    self.buf += f"    mov rax, {a}\n"
                                    self.buf += f"    mov rcx, {b}\n"
                                    self.buf += f"    cdq\n    idiv rcx\n"
                                    self.buf += f"    mov {a}, rdx\n"
                                case _:
                                    assert False, f"{kind} not implemented"
                            stack0.append(a)
                        case _:
                            self.expr(_expr, regs[i])
                            stack0.append(regs[i])
                if reg and reg != "rax":
                    self.buf += f"    mov {reg}, rax\n"
            case Ident(name):
                if name in self.consts:
                    if not reg:
                        self.buf += f"    mov rax, .L_{name}\n"
                    else:
                        self.buf += f"    mov {reg}, .L_{name}\n"
                    return
                if not reg:
                    self.load_var(name, None)
                else:
                    self.load_var(name, reg)
            case Call(name, args):
                if name in builtins_:
                    builtins_[name](self, args, reg)
                    return
                self.dbg(f"    # function call {name}")
                defn = self.defs.get(name)
                variadic = False
                for arg in defn.data['args']:
                    if type(arg) == Variadic:
                        variadic = True
                        break

                i = 0
                for arg in args:
                    match arg.ty:
                        case PrimTy(kind):
                            if kind.is_float():
                                continue
                    self.expr(arg, fn_regs[0][i])
                    i += 1

                flt_args = 0
                i = 0
                for arg in args:
                    match arg.ty:
                        case PrimTy(kind):
                            if kind.is_float():
                                self.expr(arg, fn_regs[1][i])
                                flt_args += 1
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
                if defn.data['ret_ty'].kind != PrimTyKind.Unit and reg and reg != "rax":
                    self.dbg(f"    # return {name}")
                    self.buf += f"    mov {reg}, rax\n"
            case Literal(kind, value):
                if not reg:
                    return
                self.dbg(f"    # {kind}: {value}")
                match kind:
                    case Lit.Str:
                        self.buf += f"    lea {reg}, [rip + .L__unnamed_{len(self.strings)}]\n"
                        # self.buf += f"    mov {reg}, OFFSET FLAT:str{len(self.strings)}\n"
                        self.strings.append(value)
                    case Lit.Bool:
                        self.buf += f"    mov {reg}, {1 if value == 'true' else 0}\n"
                    case Lit.Float:
                        label = self.label
                        match expr.ty.kind:
                            case PrimTyKind.F64:
                                self.buf += f"    movsd {reg}, QWORD PTR .L{label}[rip]\n"
                            case PrimTyKind.F32:
                                self.buf += f"    movss {reg}, DWORD PTR .L{label}[rip]\n"
                            case _:
                                assert False
                        self.floats.append((label, value))
                    case _:
                        self.buf += f"    mov {reg}, {value}\n"
            case Unary(kind, expr):
                self.dbg(f"    # unary {kind}")
                if not reg:
                    match kind:
                        case UnaryKind.AddrOf:
                            assert False
                        case UnaryKind.Not:
                            self.expr(expr, None)
                            match expr.ty:
                                case PrimTy(PrimTyKind.Bool):
                                    self.buf += f"    xor rax, 1\n"
                                case _:
                                    self.buf += f"    not rax\n"
                        case UnaryKind.Neg:
                            self.buf += f"    mov rbx, 0\n"
                            self.expr(expr, None)
                            self.buf += f"    sub rbx, rax\n"
                            self.buf += f"    mov rax, rbx\n"
                        case UnaryKind.Deref:
                            self.expr(expr, reg)
                            assert isinstance(expr.ty, PtrTy)
                            match expr.ty.ptr.kind:
                                case PrimTyKind.I64:
                                    self.buf += f"    mov {reg}, [{reg}]\n"
                                case _:
                                    assert False
                        case _:
                            assert False, "not implemented"
                    return
                match kind:
                    case UnaryKind.AddrOf:
                        assert type(expr.kind) == Ident
                        off = self.env.find_local(expr.kind.name)['off']
                        self.buf += f"    lea {reg}, [rbp - {off}]\n"
                    case UnaryKind.Not:
                        self.expr(expr, reg)
                        match expr.ty:
                            case PrimTy(PrimTyKind.Bool):
                                self.buf += f"    xor {reg}, 1\n"
                            case _:
                                self.buf += f"    not {reg}\n"
                    case UnaryKind.Neg:
                        self.expr(expr, reg)
                        self.buf += f"    neg {reg}\n"
                    case UnaryKind.Deref:
                        self.expr(expr, reg)
                        self.buf += f"    mov {reg}, [{reg}]\n"
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
                    case Ident() | Unary() | Call():
                        self.buf += f"    cmp rax, 0\n"
                        self.buf += f"    je .L{label_false}\n"
                    case Binary():
                        pass
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
                match expr.ty:
                    case RefTy(_):
                        self.expr(expr, reg)
                    case PrimTy(kind):
                        if kind == PrimTyKind.Raw:
                            self.expr(expr, reg)
                        else:
                            cast_map[kind][ty.kind](self, expr, reg)
                    case PtrTy(_):
                        self.expr(expr, reg)
                """
                if (expr.ty.is_int() and ty.is_int()):
                    self.expr(expr, reg)
                    return
                if (expr.ty.is_float() and expr.ty.is_float()):
                    self.expr(expr, "xmm0")
                    self.buf += f"    movq {reg}, xmm0\n"
                    return
                match ty.kind:
                    case PrimTyKind.F64:
                        self.expr(expr, None)
                        self.buf += f"    cvtsi2sd {reg}, rax\n"
                    case PrimTyKind.F32:
                        self.expr(expr, None)
                        self.buf += f"    cvtsi2ss {reg}, rax\n"
                    case PrimTyKind.I64:
                        self.expr(expr, "xmm0")
                        self.buf += f"    cvttsd2si {reg}, xmm0\n"
                    case PrimTyKind.Raw:
                        self.expr(expr, reg)
                    case _:
                        assert False
                """
            case Loop(body):
                label_start = self.label
                self.buf += f".L{label_start}:\n"
                self.gen_block(body)
                self.buf += f"    jmp .L{label_start}\n"
                for brk in self.breaks:
                    self.buf += f".L{brk}:\n"
                self.breaks = []
            case StructExpr(name, fields):
                for i, field in enumerate(fields):
                    self.expr(field.expr, regs[i])
            case _:
                assert False, f"{expr} is not implemented"

    def stmt(self, stmt: Stmt):
        match stmt.kind:
            case Let(name, ty, init):
                off, sz = self.env.def_local(name, ty)
                self.dbg(f"    # let {name}: {ty}")
                match init:
                    case Expr(Literal(kind, value)):
                        match kind:
                            case Lit.Str:
                                self.expr(init, "rax")
                                self.buf += f"    mov [rbp - {off}], rax\n"
                            case Lit.Int:
                                self.buf += f"    mov QWORD PTR [rbp - {off}], {value}\n"
                            case Lit.Bool:
                                self.buf += f"    mov BYTE PTR [rbp - {off}], {1 if value == 'true' else 0}\n"
                            case Lit.Float:
                                self.expr(init, "xmm0")
                                self.buf += f"    movsd [rbp - {off}], xmm0\n"
                            case _:
                                assert False
                    case _:
                        if ty.is_float():
                            self.expr(init, "xmm0")
                            ptr, reg = self.reg_from_sz("xmm0", sz)
                            if sz == 4:
                                self.buf += f"    movss {ptr} [rbp - {off}], {reg}\n"
                            elif sz == 8:
                                self.buf += f"    movsd {ptr} [rbp - {off}], {reg}\n"
                            else:
                                assert False
                        else:
                            self.expr(init, "rax")
                            match ty:
                                case StructTy(name, _):
                                    struct = self.types[name]
                                    for i, f in enumerate(struct.fields):
                                        ptr, reg = self.reg_from_sz(
                                            regs[i], f.ty.get_size())
                                        self.buf += f"    mov {ptr} [rbp - {f.offset}], {reg}\n"
                                        print(f.offset)
                                case _:
                                    ptr, reg = self.reg_from_sz("rax", sz)
                                    self.buf += f"    mov {ptr} [rbp - {off}], {reg}\n"
            case Break():
                break_label = self.label
                self.buf += f"    jmp .L{break_label}\n"
                stmt.kind.label = break_label
                self.breaks.append(break_label)
            case _:
                self.expr(stmt.kind, "rax")

    def gen_block(self, block: Block):
        tmp_env = self.env
        self.env = Env(tmp_env)

        if self.fn_ctx:
            args = self.fn_ctx.args
            for i, arg in enumerate(args):
                match arg:
                    case Arg(name, ty):
                        off, sz = self.env.def_local(name, ty)
                        is_float = arg.ty.is_float()
                        ptr, reg = self.reg_from_sz(fn_regs[is_float][i], sz)
                        self.dbg(f"    # {name}: {ty}")
                        if is_float:
                            self.buf += f"    movsd [rbp - {off}], {reg}\n"
                        else:
                            self.buf += f"    mov {ptr} [rbp - {off}], {reg}\n"
                    case Variadic():
                        continue
                    case _:
                        assert False, "unreachable"
        self.fn_ctx = None
        for stmt in block.stmts:
            self.stmt(stmt)
        self.env = tmp_env

    def gen(self, nodes):
        for node in nodes:
            match node:
                case Fn(name, _, ret_ty, body, is_extern, _):
                    if is_extern:
                        continue
                    self.dbg(f"# function {name}")
                    self.buf += f"\n{name}:\n    push rbp\n    mov rbp, rsp\n"
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
                    match ret_ty:
                        case PrimTy(PrimTyKind.Unit):
                            self.dbg(f"    # return {name}")
                            self.buf += f"    xor rax, rax\n"
                    self.buf += "    pop rbp\n    ret\n"
                case ExternBlock(items):
                    self.gen(items)
                case Const(name, ty, init):
                    assert isinstance(init.kind, Literal)
                    match init.kind:
                        case Literal(kind, value):
                            if not reg:
                                return
                            self.dbg(f"    # {kind}: {value}")
                            match kind:
                                case Lit.Str:
                                    self.consts[name] = {
                                        'kind': kind,
                                        'value': len(self.strings),
                                    }
                                    self.strings.append(value)
                                case Lit.Bool:
                                    self.consts[name] = {
                                        'kind': kind,
                                        'value': value,
                                    }
                                case Lit.Float:
                                    label = self.label
                                    self.consts[name] = {
                                        'kind': kind,
                                        'value': label,
                                    }
                                    self.floats.append((label, value))
                                case _:
                                    self.consts[name] = {
                                        'kind': kind,
                                        'value': value,
                                    }
                case Struct(name, _):
                    self.types[name] = node
                case _:
                    assert False, f"{node} is not implemented"

    def emit_data(self):
        import struct
        # self.buf += ".data\n"
        for i, string in enumerate(self.strings):
            self.buf += f"\n.L__unnamed_{i}:\n"
            self.buf += f"    .string {string}\n"
        for i, flt in self.floats:
            self.buf += f"\n.L{i}:\n"
            flt = struct.pack('d', float(flt))
            hi = int.from_bytes(flt[:4], 'little')
            lo = int.from_bytes(flt[4:], 'little')
            self.buf += f"    .long {hi}\n    .long {lo}\n"
        for name, const in self.consts.items():
            self.buf += f"\n.L_{name}:\n"
            match const:
                case {'kind': Lit.Str, 'value': value}:
                    self.buf += f"    .quad .L__unnamed_{value}\n"
                case {'kind': Lit.Float, 'value': value}:
                    self.buf += f"    .quad .L{value}\n"
                case {'kind': Lit.Int | Lit.Bool, 'value': value}:
                    self.buf += f"    .quad {value}\n"

    def emit(self):
        self.buf += ".intel_syntax noprefix\n.text\n.globl main\n"
        self.gen(self.ast)
        self.emit_data()
        return self.buf


def usage(arg0):
    print(f"Usage: {arg0} [command] [options] input...")
    print("\nCommands:")
    print("    build            compile the file")
    print("\nOptions:")
    print("    -h, --help       print help information\n")
    exit(1)


class Command(Enum):
    Nan = auto()
    Build = auto()


def entry(argv):
    filename = ""
    arg0 = argv[0]
    argv = argv[1:]
    command = Command.Nan
    skip_codegen = False
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
                        case "--skip-codegen":
                            skip_codegen = True
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
                case "build":
                    command = Command.Build
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
                case Command.Build:
                    lexer = lexer_from_file(filename)
                    src = lexer.program
                    file = File(filename, src)
                    tokens = list(lexer.lexfile())
                    parser = Parser(src, tokens)
                    ast = list(parser.parse())
                    tychk = TyCheck(ast)
                    if tychk.errors:
                        for err in tychk.errors:
                            err.emit(file)
                        exit(1)
                    # gen = IRGen(ast)
                    # pp(ast, max_depth=10)
                    ConstantFolder(ast).fold()
                    if skip_codegen:
                        return
                    ir = IRGen(ast).lower()
                    output = filename.split('.')[0]
                    dump_cfg(ir, output)
                    dominance_frontier(ir)
                    return
                    if 0:
                        code = Codegen(ast, tychk.defs).emit()
                        with open(f"{output}.asm", "w") as f:
                            f.write(code)

                        from subprocess import call
                        call(["as", f"{output}.asm", "-o", f"{output}.o"])
                        call(["gcc", f"{output}.o", "-o", output])
                    else:
                        Gen(ast, output).emit()
                case _:
                    assert False, "unreachable"
