import subprocess
from typing import List
from .function import *


def dump_cfg(funcs: List[FnDef | FnDecl], output: str):
    buf = ""
    for fn in funcs:
        match fn:
            case FnDef():
                buf += "    node [shape = box fontname = monospace fontsize = 10]\n"
                for block in fn.basic_blocks:
                    buf += "    bb{} [label = \"{}\" xlabel = \"bb{}\"]\n".format(
                        block.bb_id, "\\l".join(map(lambda i: str(i)[4:], block.instructions)) + "\\l", block.bb_id)

                for block in fn.basic_blocks:
                    for succ in block.succ:
                        buf += "    bb{} -> bb{};\n".format(
                            block.bb_id, succ.bb_id)

                buf = "digraph {} {{\n{}}}".format(fn.name, buf)
    with open(output + ".dot", "w") as f:
        f.write(buf)
    subprocess.call(["dot", "-Tpng", f"{output}.dot", "-o", f"{output}.png"])
    # subprocess.call(["rm", "-f", f"{output}.dot"])
