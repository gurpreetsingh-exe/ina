#!/usr/bin/python

from __future__ import annotations
import pathlib
import sys
from typing import List
import subprocess


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
        self.n = 0

    def print_results(self):
        if self.passed + self.failed != self.n:
            print("BUG: this is a bug in the test runner")

        if self.passed == self.n:
            print("  {}{}[OK]{} All {} test{} passed".format(
                Color.OKGREEN, Color.BOLD, Color.ENDC, self.n, plural(self.n)))
        else:
            print("  {} test{} passed out of {}, {} failed".format(
                  self.passed,
                  plural(self.passed),
                  self.n,
                  self.failed))


tests = Tests()


class TestResult:
    def __init__(self, stdout: str | None = None, stderr: str | None = None, exit_code: int = 0):
        self.stdout = stdout
        self.stderr = stderr
        self.exit_code = exit_code

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


def parse_expected_result(lines: List[str]) -> TestResult:
    stdout = None
    stderr = None
    exit_code = None
    if res := extract_value(lines, "STDOUT"):
        stdout = "".join(res)
    if res := extract_value(lines, "STDERR"):
        stderr = "".join(res)
    if res := extract_value(lines, "EXIT_CODE"):
        assert len(res) == 1
        exit_code = int(res[0])
    else:
        exit_code = 0
    return TestResult(stdout, stderr, exit_code)


def run_test(case: pathlib.Path):
    if case.is_dir():
        test_dir(case)
        return

    tests.n += 1
    with open(case, 'r') as f:
        expected = parse_expected_result(f.readlines())
        command = "./bin/ray build {}".format(case).split(" ")
        proc = subprocess.Popen(command)
        proc.communicate()
        if proc.returncode != 0:
            tests.failed += 1
            return
        exe = case.with_suffix("")
        proc = subprocess.Popen(
            exe, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
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


def test_dir(dirname):
    for test in dirname.iterdir():
        run_test(test)


if __name__ == "__main__":
    test_path = None
    if len(sys.argv) > 1:
        test_path = sys.argv[1]

    if not test_path:
        sys.stderr.write("error: no test path provided\n")
        exit(1)

    test = pathlib.Path(test_path)
    if not test.exists():
        sys.stderr.write("error: {} doesn't exist\n".format(test))
        exit(1)

    run_test(test)
    tests.print_results()
