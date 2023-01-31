from subprocess import call
from ir import *
from ast_lowering import LoweringContext


class CodegenContext:
    def __init__(self, in_filename: str, out_filename: str) -> None:
        self.in_filename = in_filename
        self.out_filename = out_filename


regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]


class Codegen:
    def __init__(self, ctx: CodegenContext, lowering_ctx: LoweringContext) -> None:
        self.ctx = ctx
        self.lowering_ctx = lowering_ctx
        self.defs: List[FnDef | FnDecl] = lowering_ctx.lowered_ast
        self.buf = ""
        self.strings = []
        self.reg = ""

    def emit(self):
        out = self.ctx.out_filename
        with open(out + ".asm", "w") as f:
            f.write(self.buf)
        call(["as", f"{out}.asm", "-o", f"{out}.o"])
        call(["gcc", f"{out}.o", "-o", out])

    def gen_val(self, val: Value):
        match val.kind:
            case ValueKind.SymbolId:
                self.buf += f"    lea {self.reg}, [rip + .L__unnamed_{val.data}]\n"
            case ValueKind.Imm:
                self.buf += f"    mov {self.reg}, {val.data}\n"
            case ValueKind.InstId:
                pass

    def gen_inst(self, inst: Instruction):
        match inst.kind:
            case FnCall(name, args, _):
                for i, arg in enumerate(args):
                    self.reg = regs[i]
                    self.gen_val(arg)

                if n := inst.kind.float_args:
                    self.buf += f"    mov al, {n}\n"
                else:
                    self.buf += "    xor rax, rax\n"
                self.buf += f"    call {name}\n"

    def gen_block(self, bb: BasicBlock):
        if bb.block_id != 0:
            self.buf += f".LBB{bb.block_id}:\n"
        for inst in bb.instructions:
            self.gen_inst(inst)

    def gen(self):
        # self.buf += ".intel_syntax noprefix\n.text\n.globl main\n"
        self.buf += ".intel_syntax noprefix\n.globl main\n"
        for fn in self.defs:
            match fn:
                case FnDecl(name, params, ret_ty):
                    pass
                case FnDef(name, params, ret_ty, blocks):
                    self.buf += f"\n{name}:\n"
                    stack_alignment = fn.stack_alignment
                    if stack_alignment > 0:
                        self.buf += \
                            "    push rbp\n" + \
                            "    mov rbp, rsp\n" + \
                            f"    sub rsp, {stack_alignment}\n"
                    for bb in blocks:
                        self.gen_block(bb)

                    if stack_alignment > 0:
                        self.buf += \
                            f"    add rsp, {stack_alignment}\n" + \
                            "    pop rbp\n"
                    self.buf += \
                        "    xor rax, rax\n" + \
                        "    ret\n"

        for i, string in enumerate(self.lowering_ctx.strings):
            self.buf += \
                f"\n.L__unnamed_{i}:\n" + \
                f"    .string {string}\n"


def x86_64_gas(ctx: CodegenContext, lowering_ctx: LoweringContext):
    gen = Codegen(ctx, lowering_ctx)
    gen.gen()
    gen.emit()
