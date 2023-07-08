#!/usr/bin/python

from __future__ import annotations
import pathlib
import sys
from typing import List
import subprocess
import argparse


class Color:
    OKBLUE = '\033[94m'   # ']'
    OKCYAN = '\033[96m'  # ']'
    OKGREEN = '\033[92m'  # ']'
    WARNING = '\033[93m'  # ']'
    FAIL = '\033[91m'  # ']'
    ENDC = '\033[0m'  # ']'
    BOLD = '\033[1m'  # ']'
    UNDERLINE = '\033[4m'  # ']'


def plural(n):
    return "s" if n > 1 else ""


class Tests:
    def __init__(self):
        self.passed = 0
        self.failed = 0
        self.skipped = 0
        self.fmt = 0
        self.n = 0

    def print_results(self):
        if self.passed + self.failed + self.skipped != self.n:
            print("BUG: this is a bug in the test runner")

        if self.passed == self.n:
            print("  {}{}[OK]{} All {} test{} passed".format(
                Color.OKGREEN, Color.BOLD, Color.ENDC, self.n, plural(self.n)))
        else:
            print("  {} test{} passed out of {}{}{}".format(
                  self.passed,
                  plural(self.passed),
                  self.n,
                  ", {} failed".format(self.failed) if self.failed else "",
                  ", {} skipped".format(self.skipped) if self.skipped else ""))


tests = Tests()


class TestResult:
    def __init__(self):
        self.stdout: str | None = None
        self.stderr: str | None = None
        self.exit_code: int = 0
        self.compile_fail: bool = False

    def __eq__(self, other: TestResult) -> bool:
        return self.stdout == other.stdout \
            and self.stderr == other.stderr \
            and self.exit_code == other.exit_code

    def __str__(self) -> str:
        res = ""
        res += "STDOUT: {}\n".format(repr(self.stdout))
        res += "STDERR: {}\n".format(repr(self.stderr))
        res += "EXIT_CODE: {}\n".format(self.exit_code)
        return res

    __repr__ = __str__


def extract_value(lines: List[str], value: str):
    value = "// {}: ".format(value)
    return [l.strip(value) for l in lines if l.startswith(value)]


def extract_flag(lines: List[str], value: str):
    value = "// {}".format(value)
    return any([l.strip(value) for l in lines if l.startswith(value)])


def skip(lines: List[str]):
    return any([True for l in lines if l.startswith("// SKIP")])


def parse_expected_result(lines: List[str]) -> TestResult:
    test_result = TestResult()
    if res := extract_value(lines, "STDOUT"):
        test_result.stdout = "".join(res)
    if res := extract_value(lines, "STDERR"):
        test_result.stderr = "".join(res)
    if res := extract_value(lines, "EXIT_CODE"):
        assert len(res) == 1
        test_result.exit_code = int(res[0])
    else:
        test_result.exit_code = 0
    if res := extract_flag(lines, "COMPILE_FAIL"):
        test_result.compile_fail = True
    return test_result


def fmt_test(tests: Tests, case: pathlib.Path, src: str):
    command = "./bin/ray fmt {}".format(case).split(" ")
    proc = subprocess.Popen(command, stdout=subprocess.PIPE)
    if proc.stdout:
        if stdout := proc.stdout.read().decode():
            if stdout != src:
                tests.fmt += 1


def run_test(case: pathlib.Path, options: argparse.Namespace):
    if case.is_dir():
        test_dir(case, options)
        return

    if case.suffix in {'.o'}:
        subprocess.call("rm -f {}".format(case).split(" "))
        return

    if case.suffix in {'.stderr'}:
        return

    tests.n += 1
    with open(case, 'r') as f:
        src = f.readlines()
        if skip(src):
            print(f"skipping {case}")
            tests.skipped += 1
            return
        print(f"compiling {case}")
        fmt_test(tests, case, "".join(src))
        expected = parse_expected_result(src)
        command = "./bin/ray build {} --ui-testing".format(case).split(" ")
        proc = subprocess.Popen(
            command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stderr = None
        if proc.stderr:
            stderr = proc.stderr.read().decode()
        proc.communicate()
        if expected.compile_fail:
            if proc.returncode == 0:
                tests.failed += 1
                print("compile pass: {}".format(case))
            else:
                if not stderr:
                    tests.failed += 1
                    return
                stderr_file = case.with_suffix(".stderr")
                if options.bless:
                    tests.passed += 1
                    with open(stderr_file, 'w') as f:
                        f.write(stderr)
                        return
                with open(stderr_file, 'r') as f:
                    if stderr != f.read():
                        print("failed {}".format(case))
                        tests.failed += 1
                    else:
                        tests.passed += 1
                return
        else:
            if proc.returncode != 0:
                print("compile fail: {}".format(case))
                tests.failed += 1
                return
        exe = case.with_suffix("")
        proc = subprocess.Popen(
            exe, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        print(f"executing {exe}")
        output = TestResult()
        if proc.stdout:
            if stdout := proc.stdout.read().decode():
                output.stdout = stdout
        if proc.stderr:
            if stderr := proc.stderr.read().decode():
                output.stderr = stderr
        proc.communicate()
        output.exit_code = proc.returncode
        subprocess.call("rm -f {}".format(exe).split(" "))
        if output != expected:
            print(output)
            print(expected)
            tests.failed += 1
        else:
            tests.passed += 1


def test_dir(dirname, options):
    for test in dirname.iterdir():
        run_test(test, options)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("test_dir")
    parser.add_argument("--bless", action='store_true')
    args = parser.parse_args()

    test_path = args.test_dir
    test = pathlib.Path(test_path)
    if not test.exists():
        sys.stderr.write("error: {} doesn't exist\n".format(test))
        exit(1)

    run_test(test, args)
    tests.print_results()
