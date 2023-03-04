from __future__ import annotations
from typing import Dict
from ..ir.function import *
from ..ir.inst import *
from ..ir.basic_block import *
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


cmp = {
    CmpKind.Lt: "setl",
    CmpKind.Gt: "setg",
    CmpKind.Eq: "sete",
    CmpKind.NotEq: "setne",
}

jmp = {
    CmpKind.Lt: "jge",
    CmpKind.Gt: "jle",
    CmpKind.Eq: "je",
    CmpKind.NotEq: "jne",
}


class Register:
    def __init__(self, kind: str, size: int = 8) -> None:
        self.size = size
        self.kind = kind

    @property
    def name(self) -> str:
        if self.kind in list(map(lambda r: f"xmm{r}", range(8))):
            return self.kind
        sz = 3 - int(log(self.size) / log(2))
        return getattr(RegisterKind, self.kind)[sz]

    @property
    def ptr(self) -> str:
        return {1: "byte", 2: "word", 4: "dword", 8: "qword"}[self.size]

    @property
    def is_vector(self) -> bool:
        return self.kind in list(map(lambda r: f"xmm{r}", range(8)))


fn_arg_int_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
int_regs = ["rax", "rbx", "rcx", "r10", "r11", "r12", "r13", "r14", "r15"]
vec_regs = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"]


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


used_regs = []


def alloc_reg(size) -> Register:
    for reg in int_regs:
        if reg not in used_regs:
            used_regs.append(reg)
            return Register(reg, size)
    raise NotImplemented("TODO: spill register")


def alloc_arg_reg(size) -> Register:
    for reg in fn_arg_int_regs:
        if reg not in used_regs:
            used_regs.append(reg)
            return Register(reg, size)
    raise NotImplemented("TODO: spill register")


class Gen:
    def __init__(self, ir: List[FnDef | FnDecl], output) -> None:
        self.ir = ir
        self.output = output
        self.fns = {}
        self.buf = ""
        self.fn_ctx = None
        self.label_manager = LabelManager()
        self.data_sec = DataSection(self.label_manager)
        self.reg_map: Dict[Value, Register] = {}

    def render_val(self, val: Register | IConst | None) -> str:
        match val:
            case Register():
                return val.name
            case IConst():
                match val.kind:
                    case ConstKind.Bool:
                        return str(int(bool(val.value)))
                    case ConstKind.Str:
                        reg = alloc_reg(8)
                        label = self.data_sec.add_string(val.value)
                        self.buf += f"    lea {reg.name}, [rip + {label}]\n"
                        return reg.name
                return val.value
            case _:
                assert False, val

    def size_to_ptr(self, size) -> str:
        return {1: "byte", 2: "word", 4: "dword", 8: "qword"}[size]

    def lookup(self, val: Value) -> Register | IConst:
        match val:
            case Inst():
                return self.reg_map[val]
            case IConst():
                return val
            case _:
                assert False

    def eval_const(self, const: IConst, func) -> IConst:
        match const.kind:
            case ConstKind.Int:
                const.value = func(int(const.value))
        return const

    def gen_inst(self, inst: Inst) -> Register | IConst | None:
        match inst:
            case Alloc():
                pass
            case Load():
                alloc = inst.src
                assert isinstance(alloc, Alloc)
                reg = alloc_reg(alloc.size)
                self.buf += f"    mov {reg.name}, {self.size_to_ptr(alloc.size)} ptr [rbp - {alloc.off}]\n"
                self.reg_map[inst] = reg
                return reg
            case Store():
                alloc = inst.dst
                assert isinstance(alloc, Alloc)
                val = self.lookup(inst.src)
                rendered_val = self.render_val(val)
                self.buf += f"    mov {self.size_to_ptr(alloc.size)} ptr [rbp - {alloc.off}], {rendered_val}\n"
            case Cmp():
                l = self.lookup(inst.left)
                r = self.lookup(inst.right)
                used_regs.clear()
                rendered_left = self.render_val(l)
                rendered_right = self.render_val(r)
                self.buf += f"    cmp {rendered_left}, {rendered_right}\n"
                assert isinstance(l, Register)
                l.size = 1
                r = self.render_val(l)
                self.buf += f"    {cmp[inst.kind]} {r}\n"
                self.reg_map[inst] = l
            case Add() | Sub():
                l = self.lookup(inst.left)
                r = self.lookup(inst.right)
                used_regs.clear()
                rendered_left = self.render_val(l)
                rendered_right = self.render_val(r)
                self.buf += f"    {inst.inst_name} {rendered_left}, {rendered_right}\n"
                assert isinstance(l, Register)
                self.reg_map[inst] = l
            case Jmp():
                self.buf += f"    jmp .LBB_{inst.br_id}\n"
            case Br():
                match inst.cond:
                    case Cmp():
                        self.buf += f"    {jmp[inst.cond.kind]} .LBB_{inst.bfalse}\n"
                    case _:
                        pass
            case FnCall():
                for arg in inst.args:
                    reg = alloc_arg_reg(8)
                    rendered_arg = self.render_val(self.lookup(arg))
                    self.buf += f"    mov {reg.name}, {rendered_arg}\n"
                self.buf += f"    call {inst.fn_name}\n"
                used_regs.clear()
            case _:
                assert False, inst

    def gen_block(self, block: BasicBlock):
        self.fn_ctx = None
        self.buf += f".LBB_{block.bb_id}:\n"
        for inst in block.instructions:
            self.gen_inst(inst)

    def gen(self, nodes):
        for node in nodes:
            match node:
                case FnDef(name, _, ret_ty, basic_blocks):
                    self.fns[name] = node
                    self.buf += f"\n{name}:\n"
                    size = 0
                    for block in basic_blocks:
                        for inst in block.instructions:
                            if isinstance(inst, Alloc):
                                size += inst.ty.get_size()
                    alignment = (size + 15) & ~15
                    self.buf += \
                        "    push rbp\n" + \
                        "    mov rbp, rsp\n"
                    if alignment:
                        self.buf += f"    sub rsp, {alignment}\n"
                    self.fn_ctx = node
                    for block in basic_blocks:
                        self.gen_block(block)
                    if alignment:
                        self.buf += f"    add rsp, {alignment}\n"
                    match ret_ty:
                        case PrimTy(PrimTyKind.Unit):
                            self.buf += f"    xor rax, rax\n"
                    self.buf += "    pop rbp\n"
                    self.buf += "    ret\n"
                case FnDecl():
                    pass
                case _:
                    assert False, f"{node} is not implemented"

    def emit(self):
        self.buf += ".intel_syntax noprefix\n.globl main\n"
        self.gen(self.ir)
        self.buf = self.data_sec.emit(self.buf)
        with open(f"{self.output}.asm", "w") as f:
            f.write(self.buf)
        from subprocess import call
        call(["as", f"{self.output}.asm", "-o", f"{self.output}.o"])
        call(["gcc", f"{self.output}.o", "-o", self.output])
        # call(["rm", "-f", f"{self.output}.o", f"{self.output}.asm"])
