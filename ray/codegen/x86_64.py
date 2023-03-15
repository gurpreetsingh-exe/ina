from __future__ import annotations
from typing import Callable, Dict
from ..ir.function import *
from ..ir.inst import *
from ..ir.basic_block import *
from ..Ast import *
from math import log
from dataclasses import dataclass
import struct
# from beeprint import pp


def __offset(self, args):
    assert len(args) == 2
    ptr = self.lookup(args[0])
    offset = self.lookup(args[1])
    used_regs.clear()
    rendered_ptr = self.render_val(ptr)
    rendered_offset = self.render_val(offset)
    self.buf += f"    add {rendered_ptr}, {rendered_offset}\n"
    return ptr


def align(num: int, align: int) -> int:
    return (num + align - 1) & ~(align - 1)


intrinsics: Dict[str, Callable] = {
    "offset":  __offset,
}


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
    CmpKind.Eq: "jne",
    CmpKind.NotEq: "je",
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
        self.ints = {}
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

    def add_const(self, label, const: IConst):
        self.strings[label] = const

    def emit(self, buf: str) -> str:
        buf += "\n.section .rodata\n"
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
        for label, data in self.ints.items():
            buf += f"\n{label}:\n"
            buf += f"    .quad {data}\n"
        buf += "\n.section .bss\n"
        buf += "__jmp_buf:\n    .zero 200\n"
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


class StackVar:
    __match_args__ = ('size', 'off', )

    def __init__(self, size, off) -> None:
        self.size = size
        self.off = off

    @staticmethod
    def from_alloc(alloc: Alloc) -> StackVar:
        return StackVar(alloc.size, alloc.off)


class Gen:
    def __init__(self, mod: IRModule, output) -> None:
        self.mod = mod
        self.output = output
        self.fns = {}
        self.buf = ""
        self.fn_ctx = None
        self.label_manager = LabelManager()
        self.data_sec = DataSection(self.label_manager)
        self.reg_map: Dict[Value, Register | StackVar] = {}
        self.globls: Dict[str, str] = {}
        self.func_id = 0

    def render_val(self, val: Register | StackVar | IConst | None) -> str:
        match val:
            case Register():
                return val.name
            case IConst():
                match val.kind:
                    case ConstKind.Bool:
                        return str(int(bool(val.value)))
                    case ConstKind.Str:
                        # this should be unreachable
                        reg = alloc_reg(8)
                        label = self.data_sec.add_string(val.value)
                        self.buf += f"    lea {reg.name}, [rip + {label}]\n"
                        return reg.name
                return val.value
            case StackVar(size, off):
                reg = alloc_reg(8)
                self.buf += f"    lea {reg.name}, [rbp - {off}]\n"
                return reg.name
            case _:
                assert False, val

    def size_to_ptr(self, size) -> str:
        return {1: "byte", 2: "word", 4: "dword", 8: "qword"}[size]

    def lookup(self, val: Value) -> Register | StackVar | IConst:
        match val:
            case Inst():
                return self.reg_map[val]
            case IConst():
                return val
            case Label():
                # must be an anonymous const
                reg = alloc_reg(8)
                self.buf += f"    lea {reg.name}, [rip + {val}]\n"
                return reg
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
                self.reg_map[inst] = StackVar.from_alloc(inst)
            case Load():
                alloc = inst.src
                match alloc:
                    case Alloc():
                        reg = alloc_reg(alloc.size)
                        self.buf += f"    mov {reg.name}, {self.size_to_ptr(alloc.size)} ptr [rbp - {alloc.off}]\n"
                        self.reg_map[inst] = reg
                    case Label():
                        reg = alloc_reg(8)
                        self.buf += f"    mov {reg.name}, [rip + {alloc}]\n"
                        self.reg_map[inst] = reg
                    case _:
                        reg = alloc_reg(8)
                        self.buf += f"    lea {reg.name}, [rip + {alloc}]\n"
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
            case Add() | Sub() | Mul():
                l = self.lookup(inst.left)
                r = self.lookup(inst.right)
                used_regs.clear()
                rendered_left = self.render_val(l)
                rendered_right = self.render_val(r)
                self.buf += f"    {inst.inst_name} {rendered_left}, {rendered_right}\n"
                assert isinstance(l, Register)
                self.reg_map[inst] = l
            case Jmp():
                self.buf += f"    jmp .LBB{self.func_id}_{inst.br_id}\n"
            case Br():
                match inst.cond:
                    case Cmp():
                        self.buf += f"    {jmp[inst.cond.kind]} .LBB{self.func_id}_{inst.bfalse}\n"
                    case _:
                        pass
            case FnCall():
                if inst.fn_name in intrinsics:
                    self.reg_map[inst] = intrinsics[inst.fn_name](
                        self, inst.args)
                else:
                    for arg in inst.args:
                        lookup_val = self.lookup(arg)
                        rendered_arg = self.render_val(lookup_val)
                        if isinstance(lookup_val, Register):
                            reg = alloc_arg_reg(lookup_val.size)
                        else:
                            reg = alloc_arg_reg(8)
                        self.buf += f"    mov {reg.name}, {rendered_arg}\n"
                    self.buf += f"    call {inst.fn_name}\n"
                    used_regs.clear()
                    reg = alloc_reg(8)
                    self.reg_map[inst] = reg
            case Ret():
                reg = Register("rax", 8)
                if inst.val:
                    value = self.lookup(inst.val)
                    if isinstance(value, Register) and value.kind == "rax":
                        self.buf += "    ret\n"
                        return value
                    val = self.render_val(value)
                    self.buf += f"    mov {reg.name}, {val}\n"
                else:
                    self.buf += f"    xor {reg.name}, {reg.name}\n"
                self.buf += "    ret\n"
            case _:
                assert False, inst

    def gen_block(self, block: BasicBlock):
        self.fn_ctx = None
        self.buf += f".LBB{self.func_id}_{block.bb_id}:\n"
        for inst in block.instructions:
            self.gen_inst(inst)
        used_regs.clear()

    def gen(self, nodes):
        # 0x9b = DW_EH_PE_pcrel | DW_EH_PE_indirect | DW_EH_PE_sdata4
        for node in nodes:
            match node:
                case FnDef(name, args, ret_ty, basic_blocks):
                    self.fns[name] = node
                    self.buf += f"\n{name}:\n"
                    alignment = 0
                    for block in basic_blocks:
                        for inst in block.instructions:
                            if isinstance(inst, Alloc):
                                if isinstance(inst.ty, ArrayTy):
                                    alignment = align(
                                        alignment + inst.ty.get_size(), 8)
                                else:
                                    size = inst.ty.get_size()
                                    alignment = (
                                        alignment + size * 2 - 1) & ~(size - 1)
                    alignment = align(alignment, 16)
                    for arg in args:
                        self.reg_map[arg] = alloc_arg_reg(8)
                    used_regs.clear()
                    self.buf += \
                        "    push rbp\n"
                    self.buf += "    mov rbp, rsp\n"
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
                    self.func_id += 1
                case FnDecl():
                    pass
                case _:
                    assert False, f"{node} is not implemented"

    def emit(self):
        self.buf += ".intel_syntax noprefix\n.globl main\n"
        for _, value in self.mod.consts.items():
            label, const = value
            self.data_sec.ints[label] = const
        for label, const in self.mod.anon_consts.items():
            self.data_sec.strings[label] = const
        self.gen(self.mod.defs)
        self.buf = self.data_sec.emit(self.buf)
        with open(f"{self.output}.asm", "w") as f:
            f.write(self.buf)
        from subprocess import call
        call(["as", f"{self.output}.asm", "-o", f"{self.output}.o"])
        call(
            ["gcc", "-lunwind", f"{self.output}.o", "-o", self.output])
        # call(["gcc", f"{self.output}.o", "-o", self.output])
        # call(["rm", "-f", f"{self.output}.o", f"{self.output}.asm"])
