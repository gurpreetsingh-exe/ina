#!/usr/bin/python

import os
import pathlib
import subprocess
from ray import *
from ast_lowering import IRGen, LoweringContext


def split_file(filepath):
    inp = ""
    with open(filepath, 'r') as f:
        inp = f.read()
    return inp.split("// STDOUT: ")


def test_comp_fail(filepath):
    src = ""
    with open(filepath, 'r') as f:
        src = f.read()
    passed = True
    lexer = lexer_from_src(src, filepath)
    src = lexer.program
    tokens = list(lexer.lexfile())
    parser = Parser(src, tokens)
    ast = list(parser.parse())
    errors = TyCheck(ast).errors
    if errors:
        passed = False
    return passed


def test_behavior(filepath):
    src, stdout = split_file(filepath)
    stdout = stdout.replace("// ", "")
    lexer = lexer_from_src(src, filepath)
    src = lexer.program
    tokens = list(lexer.lexfile())
    parser = Parser(src, tokens)
    ast = list(parser.parse())
    tychk = TyCheck(ast)
    output = filepath.stem
    if 0:
        code = Codegen(ast, tychk.defs).emit()
        with open(f"{output}.asm", "w") as f:
            f.write(code)
        subprocess.call(["as", f"{output}.asm", "-o", f"{output}.o"])
        subprocess.call(["gcc", f"{output}.o", "-o", output])
    else:
        Gen(ast, output).emit()
    proc = subprocess.Popen(
        [f"./{output}"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    lines = "".join([i.decode('utf-8') for i in proc.stdout.readlines()])
    proc.communicate()
    failed = False
    if proc.returncode or lines != stdout:
        print(filepath, lines)
        failed = True
    subprocess.call(['rm', "-f", output, f"{output}.asm", f"{output}.o"])
    return failed


def run_tests():
    root = pathlib.Path(__file__).parent.parent
    tests_dir = os.path.join(root, "tests")
    comp_fail = os.path.join(tests_dir, "compile_fail")
    behavior = os.path.join(tests_dir, "behavior")

    failed = 0
    for file in pathlib.Path(comp_fail).iterdir():
        failed += test_comp_fail(file)
    for file in pathlib.Path(behavior).iterdir():
        failed += test_behavior(file)
    if failed:
        print(f"  {failed} tests failed")
    else:
        print("  All tests passed")


if __name__ == "__main__":
    run_tests()
