from __future__ import annotations
from enum import Enum, auto
from typing import Dict, Any, Tuple
from ..Ast import *
from math import log
from dataclasses import dataclass
import struct
from beeprint import pp


@dataclass
class RegisterKind:
    rax = ["rax", "eax", "ax", "al"]
    rbx = ["rbx", "ebx", "bx", "bl"]
    rcx = ["rcx", "ecx", "cx", "cl"]
    rdx = ["rdx", "edx", "dx", "dl"]
    rsi = ["rsi", "esi", "si", "sil"]
    rdi = ["rdi", "edi", "di", "dil"]
    r8 = ["r8", "r8d", "r8w", "r8b"]
    r9 = ["r9", "r9d", "r9w", "r9b"]
    r10 = ["r10", "r10d", "r10w", "r10b"]
    r11 = ["r11", "r11d", "r11w", "r11b"]
    r12 = ["r12", "r12d", "r12w", "r12b"]
    r13 = ["r13", "r13d", "r13w", "r13b"]
    r14 = ["r14", "r14d", "r14w", "r14b"]
    r15 = ["r15", "r15d", "r15w", "r15b"]
    xmm0 = "xmm0"
    xmm1 = "xmm1"
    xmm2 = "xmm2"
    xmm3 = "xmm3"
    xmm4 = "xmm4"
    xmm5 = "xmm5"
    xmm6 = "xmm6"
    xmm7 = "xmm7"


class Register:
    def __init__(self, kind: str, size: int) -> None:
        self.size = size
        self.kind = kind

    @property
    def name(self) -> str:
        if self.kind in list(map(lambda r: f"xmm{r}", range(8))):
            return self.kind
        # 1 => 0
        # 2 => 1
        # 4 => 2
        # 8 => 3
        sz = 3 - int(log(self.size) / log(2))
        return getattr(RegisterKind, self.kind)[sz]

    @property
    def ptr(self) -> str:
        return {1: "byte", 2: "word", 4: "dword", 8: "qword"}[self.size]

    @property
    def is_vector(self) -> bool:
        return self.kind in list(map(lambda r: f"xmm{r}", range(8)))


class Imm(Enum):
    Int = auto()
    Float = auto()
    Str = auto()


class Immediate:
    __match_args__ = ("kind", "value", )

    def __init__(self, kind: Imm, value, size: int) -> None:
        self.kind = kind
        self.value = value
        self.size = size

    @classmethod
    def from_lit(cls, expr):
        lit = expr.kind
        if expr.ty:
            size = expr.ty.get_size()
        else:
            size = 8
        match lit.kind:
            case Lit.Int:
                return cls(Imm.Int, lit.value, size)
            case Lit.Float:
                return cls(Imm.Float, lit.value, size)
            case Lit.Str:
                return cls(Imm.Str, lit.value, size)
            case _:
                assert False, lit.kind


class Value:
    def __init__(self, kind: Register | Immediate):
        self.kind = kind


fn_arg_int_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
int_regs = ["rax", "rbx", "rcx", "r10", "r11", "r12", "r13", "r14", "r15"]
vec_regs = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"]


class RegisterManager:
    def __init__(self) -> None:
        self.used = []
        self.args = []

    def free_regs(self) -> None:
        self.args.clear()

    def free(self) -> None:
        self.used.clear()

    def find_int_arg_reg(self) -> str | None:
        for reg in fn_arg_int_regs:
            if not reg in self.args:
                return reg

    def find_int_reg(self) -> str | None:
        for reg in int_regs:
            if not reg in self.used:
                return reg

    def find_vec_reg(self) -> str | None:
        for reg in vec_regs:
            if not reg in self.used:
                return reg

    def alloc_reg(self, ty: Ty) -> Register | None:
        match ty:
            case PrimTy(kind):
                match kind:
                    case PrimTyKind.I64 | PrimTyKind.U64 | PrimTyKind.ISize | PrimTyKind.USize:
                        reg = self.find_int_reg()
                        if reg:
                            self.used.append(reg)
                            return Register(reg, 8)
                    case PrimTyKind.F64:
                        reg = self.find_vec_reg()
                        if reg:
                            self.used.append(reg)
                            return Register(reg, 8)
                    case PrimTyKind.F32:
                        reg = self.find_vec_reg()
                        if reg:
                            self.used.append(reg)
                            return Register(reg, 4)
                    case PrimTyKind.I32 | PrimTyKind.U32:
                        reg = self.find_int_reg()
                        if reg:
                            self.used.append(reg)
                            return Register(reg, 4)
                    case PrimTyKind.I16 | PrimTyKind.U16:
                        reg = self.find_int_reg()
                        if reg:
                            self.used.append(reg)
                            return Register(reg, 2)
                    case PrimTyKind.I8 | PrimTyKind.U8 | PrimTyKind.Bool | PrimTyKind.Char:
                        reg = self.find_int_reg()
                        if reg:
                            self.used.append(reg)
                            return Register(reg, 1)
                    case _:
                        assert kind
            case PtrTy(_) | RefTy(_) | FnTy(_):
                reg = self.find_int_reg()
                if reg:
                    self.used.append(reg)
                    return Register(reg, 8)
            case _:
                assert False, ty

    def alloc_arg_reg(self, ty: Ty) -> Register | None:
        match ty:
            case PrimTy(kind):
                match kind:
                    case PrimTyKind.I64 | PrimTyKind.U64 | PrimTyKind.ISize | PrimTyKind.USize | PrimTyKind.Str | PrimTyKind.Raw:
                        reg = self.find_int_arg_reg()
                        if reg:
                            self.args.append(reg)
                            return Register(reg, 8)
                    case PrimTyKind.F64:
                        reg = self.find_vec_reg()
                        if reg:
                            self.args.append(reg)
                            return Register(reg, 8)
                    case PrimTyKind.F32:
                        reg = self.find_vec_reg()
                        if reg:
                            self.args.append(reg)
                            return Register(reg, 4)
                    case PrimTyKind.I32 | PrimTyKind.U32:
                        reg = self.find_int_arg_reg()
                        if reg:
                            self.args.append(reg)
                            return Register(reg, 4)
                    case PrimTyKind.I16 | PrimTyKind.U16:
                        reg = self.find_int_arg_reg()
                        if reg:
                            self.args.append(reg)
                            return Register(reg, 2)
                    case PrimTyKind.I8 | PrimTyKind.U8 | PrimTyKind.Bool | PrimTyKind.Char:
                        reg = self.find_int_arg_reg()
                        if reg:
                            self.args.append(reg)
                            return Register(reg, 1)
                    case _:
                        assert kind
            case PtrTy(_) | RefTy(_) | FnTy(_):
                reg = self.find_int_arg_reg()
                if reg:
                    self.args.append(reg)
                    return Register(reg, 8)
            case _:
                assert False, ty


class Env:
    def __init__(self, parent: Env | None = None):
        self.parent = parent
        self.bindings = []
        self.stack_offset = self.parent.stack_offset if self.parent else 0

    def def_local(self, name, ty) -> Tuple[int, int]:
        size = ty.get_size()
        self.stack_offset = (self.stack_offset + size * 2 - 1) & ~(size - 1)
        self.bindings.append({'name': name,
                              'off': self.stack_offset, 'ty': ty})
        return self.stack_offset, size

    def find_local(self, name) -> Dict[str, Any] | None:
        for var in self.bindings:
            if var['name'] == name:
                return var
        if self.parent:
            return self.parent.find_local(name)


class LabelManager:
    def __init__(self) -> None:
        self.label_id = 0

    @property
    def unnamed_label(self) -> str:
        self.label_id += 1
        return f".L__unnamed_{self.label_id}"


class DataSection:
    def __init__(self, label_manager: LabelManager) -> None:
        self.label_manager = label_manager
        self.floats = {}
        self.strings = {}

    def add_float_64(self, value) -> str:
        label = self.label_manager.unnamed_label
        flt = struct.pack('d', float(value))
        self.floats[label] = flt
        return label

    def add_float_32(self, value) -> str:
        label = self.label_manager.unnamed_label
        flt = struct.pack('f', float(value))
        self.floats[label] = flt
        return label

    def add_string(self, value) -> str:
        label = self.label_manager.unnamed_label
        self.strings[label] = value
        return label

    def emit(self, buf: str) -> str:
        for label, data in self.floats.items():
            bytez = len(data)
            data = int.from_bytes(data, 'little')
            buf += f"\n{label}:\n"
            if bytez == 8:
                float_ = struct.unpack('d', data.to_bytes(8, 'little'))[0]
                buf += f"    .quad {data} # {float_}\n"
            elif bytez == 4:
                float_ = struct.unpack('f', data.to_bytes(4, 'little'))[0]
                buf += f"    .long {data} # {float_}\n"
        for label, data in self.strings.items():
            buf += f"\n{label}:\n"
            buf += f"    .string {data}\n"
        return buf


class Gen:
    def __init__(self, ast, output) -> None:
        self.ast = ast
        self.output = output
        self.reg_manager = RegisterManager()
        self.env = Env()
        self.fns = {}
        self.buf = ""
        self.fn_ctx = None
        self.label_manager = LabelManager()
        self.data_sec = DataSection(self.label_manager)

    def store_reg(self, off: int, reg: Register):
        if reg.is_vector:
            match reg.size:
                case 4:
                    self.buf += f"    movss {reg.ptr} ptr [rbp - {off}], {reg.name}\n"
                case 8:
                    self.buf += f"    movsd {reg.ptr} ptr [rbp - {off}], {reg.name}\n"
                case _:
                    assert False, "f32, f64"
        else:
            self.buf += f"    mov {reg.ptr} ptr [rbp - {off}], {reg.name}\n"

    def store_val(self, off: int, val: Immediate):
        match val.kind:
            case Imm.Int:
                self.buf += f"    mov qword ptr [rbp - {off}], {val.value}\n"
            case Imm.Float:
                if val.size == 8:
                    label = self.data_sec.add_float_64(val.value)
                elif val.size == 4:
                    label = self.data_sec.add_float_32(val.value)
                self.buf += f"    mov rax, [rip + {label}]\n"
                self.buf += f"    mov qword ptr [rbp - {off}], rax\n"
            case Imm.Str:
                label = self.data_sec.add_string(val.value)
                self.buf += f"    lea rax, [rip + {label}]\n"
                self.buf += f"    mov qword ptr [rbp - {off}], rax\n"
            case _:
                assert False, val.kind

    def push_var(self, name):
        pass

    def load(self, var, is_arg=False) -> Register:
        if is_arg:
            reg = self.reg_manager.alloc_arg_reg(var['ty'])
        else:
            reg = self.reg_manager.alloc_reg(var['ty'])
        if not reg:
            assert False
        if reg.is_vector:
            match reg.size:
                case 4:
                    self.buf += f"    movss {reg.name}, {reg.ptr} ptr [rbp - {var['off']}]\n"
                    self.buf += f"    cvtss2sd {reg.name}, {reg.name}\n"
                case 8:
                    self.buf += f"    movsd {reg.name}, {reg.ptr} ptr [rbp - {var['off']}]\n"
                case _:
                    assert False, "f32, f64"
        else:
            self.buf += f"    mov {reg.name}, {reg.ptr} ptr [rbp - {var['off']}]\n"
        return reg

    def gen_block(self, block):
        tmp_env = self.env
        self.env = Env(tmp_env)

        if self.fn_ctx:
            args = self.fn_ctx.args
            for i, arg in enumerate(args):
                match arg:
                    case Arg(name, ty):
                        off, _ = self.env.def_local(name, ty)
                        reg = self.reg_manager.alloc_arg_reg(ty)
                        if reg:
                            self.store_reg(off, reg)
                        else:
                            self.buf += \
                                f"    mov rax, qword ptr [rbp + {i * 8}]\n"
                            self.store_reg(off, Register("rax", 8))
                    case Variadic():
                        break
                    case _:
                        assert False, "unreachable"
        self.reg_manager.free_regs()
        self.fn_ctx = None
        for stmt in block.stmts:
            self.stmt(stmt)
        self.env = tmp_env

    def load_addr(self, var, is_arg=False) -> Register:
        if is_arg:
            reg = self.reg_manager.alloc_arg_reg(RefTy(var['ty']))
        else:
            reg = self.reg_manager.alloc_reg(RefTy(var['ty']))
        if not reg:
            assert False
        self.buf += f"    lea {reg.name}, {reg.ptr} ptr [rbp - {var['off']}]\n"
        return reg

    def arg_expr(self, expr) -> Register:
        match expr.kind:
            case Literal(kind, value):
                ty = None
                match kind:
                    case Lit.Int:
                        ty = PrimTy(PrimTyKind.I64)
                    case Lit.Float:
                        ty = PrimTy(PrimTyKind.F64)
                    case Lit.Str:
                        ty = PrimTy(PrimTyKind.Str)
                    case _:
                        assert False, kind
                reg = self.reg_manager.alloc_arg_reg(ty)
                if not reg:
                    assert False, "register overflow"
                self.store(reg, Value(Immediate.from_lit(expr)))
                return reg
            case Unary(kind, expr):
                match kind:
                    case UnaryKind.AddrOf:
                        var = self.env.find_local(expr.kind.name)
                        return self.load_addr(var, True)
                    case _:
                        assert False, kind
            case Ident(name):
                var = self.env.find_local(name)
                if var:
                    return self.load(var, True)
                elif name in self.fns:
                    reg = self.reg_manager.alloc_arg_reg(FnTy.dummy())
                    if not reg:
                        assert False, "register overflow"
                    self.buf += f"    lea {reg.name}, [rip + {name}]\n"
                    return reg
                else:
                    assert False
            case Call(name, args):
                floats = 0
                for arg in args:
                    reg = self.arg_expr(arg)
                    floats += reg.is_vector
                self.reg_manager.free_regs()
                if floats:
                    self.store(Register("rax", 4),
                               Value(Immediate(Imm.Int, floats, 8)))
                self.buf += f"    call {name}\n"
                return Register("rax", 8)
            case _:
                assert False, expr.kind

    def expr(self, expr) -> Value:
        match expr.kind:
            case Literal(_):
                return Value(Immediate.from_lit(expr))
            case Unary(kind, expr):
                match kind:
                    case UnaryKind.AddrOf:
                        var = self.env.find_local(expr.kind.name)
                        return Value(self.load_addr(var))
                    case _:
                        assert False, kind
            case Ident(name):
                var = self.env.find_local(name)
                if var:
                    return Value(self.load(var))
                elif name in self.fns:
                    reg = self.reg_manager.alloc_arg_reg(FnTy.dummy())
                    if not reg:
                        assert False, "register overflow"
                    self.buf += f"    lea {reg.name}, [rip + {name}]\n"
                    return Value(reg)
                else:
                    assert False
            case Call(name, args):
                floats = 0
                for arg in args:
                    reg = self.arg_expr(arg)
                    floats += reg.is_vector
                self.reg_manager.free_regs()
                if floats:
                    self.store(Register("rax", 4),
                               Value(Immediate(Imm.Int, floats, 8)))
                var = self.env.find_local(name)
                if var and type(var['ty']) == FnTy:
                    self.buf += f"    mov rax, [rbp - {var['off']}]\n"
                    self.buf += f"    call rax\n"
                else:
                    self.buf += f"    call {name}\n"
                return Value(Register("rax", 8))
            case _:
                assert False, expr.kind

    def store(self, r0: Register, r1: Value):
        match r := r1.kind:
            case Register():
                if r.is_vector:
                    self.buf += f"    mov rax, {r.name}\n"
                    self.buf += f"    mov {r0.name}, rax\n"
                else:
                    self.buf += f"    mov {r0.name}, {r.name}\n"
            case Immediate(kind, value):
                match kind:
                    case Imm.Int:
                        self.buf += f"    mov {r0.name}, {value}\n"
                    case Imm.Str:
                        label = self.data_sec.add_string(value)
                        self.buf += f"    lea {r0.name}, [rip + {label}]\n"
                    case _:
                        assert False
            case _:
                assert False

    def stmt(self, stmt):
        match stmt.kind:
            case Let(name, ty, init):
                off, _ = self.env.def_local(name, ty)
                match value := self.expr(init).kind:
                    case Register():
                        self.store_reg(off, value)
                    case Immediate():
                        self.store_val(off, value)
                    case _:
                        assert False, value
            case Break():
                pass
            case Expr():
                self.expr(stmt.kind)

    def gen(self, nodes):
        for node in nodes:
            match node:
                case Fn(name, _, ret_ty, body, is_extern, _):
                    self.fns[name] = node
                    if is_extern:
                        continue
                    self.buf += f"\n{name}:\n"
                    alignment = node.stack_alignment
                    self.buf += \
                        "    push rbp\n" + \
                        "    mov rbp, rsp\n"
                    if alignment:
                        self.buf += f"    sub rsp, {alignment}\n"
                    assert body != None
                    self.fn_ctx = node
                    self.gen_block(body)
                    if alignment:
                        self.buf += f"    add rsp, {alignment}\n"
                    match ret_ty:
                        case PrimTy(PrimTyKind.Unit):
                            self.buf += f"    xor rax, rax\n"
                    self.buf += "    pop rbp\n"
                    self.buf += "    ret\n"
                case ExternBlock(items):
                    self.gen(items)
                case _:
                    assert False, f"{node} is not implemented"

    def emit(self):
        self.buf += ".intel_syntax noprefix\n.globl main\n"
        self.gen(self.ast)
        self.buf = self.data_sec.emit(self.buf)
        with open(f"{self.output}.asm", "w") as f:
            f.write(self.buf)
        from subprocess import call
        call(["as", f"{self.output}.asm", "-o", f"{self.output}.o"])
        call(["gcc", f"{self.output}.o", "-o", self.output])
        # call(["rm", "-f", f"{self.output}.o", f"{self.output}.asm"])
