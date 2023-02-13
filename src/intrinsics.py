from __future__ import annotations


def __builtin_offset(self, args, reg):
    self.expr(args[0], reg)
    self.expr(args[1], "rbx")
    self.buf += f"    add {reg}, rbx\n"


builtins_ = {
    "offset": __builtin_offset,
}
