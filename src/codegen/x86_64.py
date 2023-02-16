from __future__ import annotations
from typing import Dict
from enum import Enum, auto
from Ast import *
from math import log
from dataclasses import dataclass


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


class Value:
    def __init__(self, kind: Register | Literal):
        self.kind = kind


fn_arg_int_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
int_regs = ["rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8",
            "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
vec_regs = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"]


class RegisterManager:
    def __init__(self) -> None:
        self.used = []

    def free_regs(self) -> None:
        self.used.clear()

    def find_int_arg_reg(self) -> str | None:
        for reg in fn_arg_int_regs:
            if not reg in self.used:
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

    def alloc_arg_reg(self, ty: Ty) -> Register | None:
        match ty:
            case PrimTy(kind):
                match kind:
                    case PrimTyKind.I64 | PrimTyKind.U64 | PrimTyKind.ISize | PrimTyKind.USize:
                        reg = self.find_int_arg_reg()
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
                        reg = self.find_int_arg_reg()
                        if reg:
                            self.used.append(reg)
                            return Register(reg, 4)
                    case PrimTyKind.I16 | PrimTyKind.U16:
                        reg = self.find_int_arg_reg()
                        if reg:
                            self.used.append(reg)
                            return Register(reg, 2)
                    case PrimTyKind.I8 | PrimTyKind.U8 | PrimTyKind.Bool | PrimTyKind.Char:
                        reg = self.find_int_arg_reg()
                        if reg:
                            self.used.append(reg)
                            return Register(reg, 1)
                    case _:
                        assert kind


class Env:
    def __init__(self, parent: Env | None = None):
        self.parent = parent
        self.bindings = []
        self.stack_offset = self.parent.stack_offset if self.parent else 0

    def def_local(self, name, ty) -> Tuple[int, int]:
        size = ty.get_size()
        self.stack_offset = (self.stack_offset + size * 2 - 1) & ~(size - 1)
        self.bindings.append({'name': name,
                              'off': self.stack_offset, 'size': size})
        return self.stack_offset, size

    def find_local(self, name) -> Dict[str, Any]:
        for var in self.bindings:
            if var['name'] == name:
                return var
        if self.parent:
            return self.parent.find_local(name)
        else:
            assert False


class Gen:
    def __init__(self, ast, output) -> None:
        self.ast = ast
        self.output = output
        self.reg_manager = RegisterManager()
        self.env = Env()
        self.buf = ""
        self.fn_ctx = None

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

    def store_val(self, off: int, val: Literal):
        match val.kind:
            case Lit.Int:
                self.buf += f"    mov qword ptr [rbp - {off}], {val.value}\n"
            case Lit.Float:
                self.buf += f"    movsd qword ptr [rbp - {off}], {val.value}\n"

    def push_var(self, name):
        pass

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

    def expr(self, expr: Expr) -> Value:
        match expr.kind:
            case Literal(kind, value):
                return Value(expr.kind)
            case _:
                assert False, expr.kind

    def stmt(self, stmt):
        match stmt.kind:
            case Let(name, ty, init):
                off, _ = self.env.def_local(name, ty)
                match value := self.expr(init).kind:
                    case Register():
                        self.store_reg(off, value)
                    case Literal():
                        self.store_val(off, value)

    def gen(self, nodes):
        for node in nodes:
            match node:
                case Fn(name, _, ret_ty, body, is_extern, _):
                    if is_extern:
                        continue
                    self.buf += f"\n{name}:\n"
                    alignment = node.stack_alignment
                    if alignment:
                        self.buf += \
                            "    push rbp\n" + \
                            "    mov rbp, rsp\n" + \
                            f"    sub rsp, {alignment}\n"
                    assert body != None
                    self.fn_ctx = node
                    self.gen_block(body)
                    if alignment:
                        self.buf += f"    add rsp, {alignment}\n"
                    match ret_ty:
                        case PrimTy(PrimTyKind.Unit):
                            self.buf += f"    xor rax, rax\n"
                    if alignment:
                        self.buf += "    pop rbp\n"
                    self.buf += "    ret\n"
                case ExternBlock(items):
                    self.gen(items)
                case _:
                    assert False, f"{node} is not implemented"

    def emit(self):
        self.buf += ".intel_syntax noprefix\n.globl main\n"
        self.gen(self.ast)
        with open(f"{self.output}.asm", "w") as f:
            f.write(self.buf)
        from subprocess import call
        call(["as", f"{self.output}.asm", "-o", f"{self.output}.o"])
        call(["gcc", f"{self.output}.o", "-o", self.output])
