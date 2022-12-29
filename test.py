#!/usr/bin/python

import os
import pathlib
import subprocess
from src.ray import *


def split_file(filepath):
    inp = ""
    with open(filepath, 'r') as f:
        inp = f.read()
    return inp.split("\\o\n")


def test_comp_fail(filepath):
    src, stdout = split_file(filepath)
    passed = True
    try:
        lexer = lexer_from_src(src, filepath)
        src = lexer.program
        tokens = list(lexer.lexfile())
        parser = Parser(src, tokens)
        ast = list(parser.parse())
        TyCheck(ast)
    except AssertionError:
        passed = False
        if errors[0] != stdout:
            print(f"test failed {filepath}")
        errors.pop()
    return passed


def test_behavior(filepath):
    src, stdout = split_file(filepath)
    lexer = lexer_from_src(src, filepath)
    src = lexer.program
    tokens = list(lexer.lexfile())
    parser = Parser(src, tokens)
    ast = list(parser.parse())
    tychk = TyCheck(ast)
    code = Codegen(ast, tychk.defs).emit()
    output = filepath.stem
    with open(f"{output}.asm", "w") as f:
        f.write(code)
    subprocess.call(["as", f"{output}.asm", "-o", f"{output}.o"])
    subprocess.call(["gcc", f"{output}.o", "-o", output])
    proc = subprocess.Popen([f"./{output}"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    lines = "".join([i.decode('utf-8') for i in proc.stdout.readlines()])
    proc.communicate()
    failed = False
    if proc.returncode or lines != stdout:
        print(lines)
        failed = True
    subprocess.call(['rm', "-f", output, f"{output}.asm", f"{output}.o"])
    return failed


def run_tests():
    root = pathlib.Path(__file__).parent
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

