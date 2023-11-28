#!/usr/bin/python

from __future__ import annotations
import pathlib
import sys
import os
from typing import List, Dict
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
        self.passed = []
        self.failed = []
        self.skipped = []
        self.fmt = 0
        self.n = []
        self.collected_tests = 0
        self.reported_fails = 0
        self.reported_skips = 0
        self.units: Dict[pathlib.Path, pathlib.Path] = {}
        self.term_cols = os.get_terminal_size().columns - 1

    def report(self, kind, name):
        j = len(self.n)
        count = self.collected_tests
        if kind == 'FAILED':
            kind = Color.BOLD + Color.FAIL + kind + Color.ENDC
            self.reported_fails += 1
        elif kind == 'SKIPPED':
            kind = Color.BOLD + Color.WARNING + kind + Color.ENDC
            self.reported_skips += 1
        else:
            assert False

        print("\x1b[K[{}/{}] {}... {}".format(j, count, name, kind),
              file=sys.stdout, flush=True)

    def progressbar(self, prefix="", out=sys.stdout):
        j = len(self.n)
        count = self.collected_tests
        if not self.n:
            return
        last = self.n[-1]
        if len(self.failed) != self.reported_fails:
            self.report('FAILED', self.failed[-1])
        elif len(self.skipped) != self.reported_skips:
            self.report('SKIPPED', self.skipped[-1])

        print("\x1b[K{} [{}/{}] {}".format(prefix, j, count, last),
              end='\r', file=out, flush=True)

    def print_results(self):
        n = len(self.n)
        if len(self.passed + self.failed + self.skipped) != n:
            print("BUG: this is a bug in the test runner")

        if len(self.passed) == n:
            print("  {}{}[OK]{} All {} test{} passed".format(
                Color.OKGREEN, Color.BOLD, Color.ENDC, n, plural(n)))
        else:
            print("\n  {} test{} passed out of {}{}{}".format(
                  len(self.passed),
                  plural(len(self.passed)),
                  n,
                  ", {} failed".format(len(self.failed)) if len(
                      self.failed) else "",
                  ", {} skipped\n".format(len(self.skipped)) if len(self.skipped) else ""))


tests = Tests()


class TestResult:
    def __init__(self):
        self.stdout: str | None = None
        self.stderr: str | None = None
        self.exit_code: int = 0
        self.compile_fail: bool = False
        self.compile_pass: bool = False

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
    return [l.replace(value, "") for l in lines if l.startswith(value)]


def extract_flag(lines: List[str], value: str):
    value = "// {}".format(value)
    return any([l.strip(value) for l in lines if l.startswith(value)])


def skip(lines: List[str]):
    return any([True for l in lines if l.startswith("// SKIP")])


def dummy(lines: List[str]):
    return any([True for l in lines if l.startswith("// DUMMY")])


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
    elif res := extract_flag(lines, "COMPILE_PASS"):
        test_result.compile_pass = True
    return test_result


def fmt_test(tests: Tests, case: pathlib.Path, src: str):
    command = "./bin/ina fmt {}".format(case).split(" ")
    proc = subprocess.Popen(command, stdout=subprocess.PIPE)
    if proc.stdout:
        if stdout := proc.stdout.read().decode():
            if stdout != src:
                tests.fmt += 1


def run_test(case: pathlib.Path, options: argparse.Namespace):
    tests.progressbar(prefix="  ")

    if case.is_dir():
        test_dir(case, options)
        return

    if case.suffix in {'.o'}:
        subprocess.call("rm -f {}".format(case).split(" "))
        return

    if case.suffix in {'.stderr'}:
        return

    with open(case, 'r') as f:
        src = [f.readline()]
        if dummy(src):
            return

        src += f.readlines()

        tests.n.append(case)
        if skip(src):
            tests.skipped.append(case)
            return
        fmt_test(tests, case, "".join(src))
        expected = parse_expected_result(src)

        if res := extract_value(src, "REQUIRES"):
            res = list(map(lambda r: r.replace("\n", ""), res))
            for r in res:
                unit = case.with_name("lib" + r).with_suffix(".o")
                unit_src = case.with_name(r).with_suffix(".ray")
                if unit_src in tests.units:
                    continue
                tests.units[unit_src] = unit
                subprocess.call("./bin/ina build {} --emit=unit".format(
                    unit_src).split(" "))

        command = "./bin/ina build {} --ui-testing".format(case).split(" ")
        proc = subprocess.Popen(
            command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stderr = None
        if proc.stderr:
            stderr = proc.stderr.read().decode()
        proc.communicate()
        exe = case.with_suffix("")
        if expected.compile_fail:
            if proc.returncode == 0:
                tests.failed.append(case)
                return
            else:
                if not stderr:
                    tests.failed.append(case)
                    return
                stderr_file = case.with_suffix(".stderr")
                if options.bless:
                    tests.failed.append(case)
                    with open(stderr_file, 'w') as f:
                        f.write(stderr)
                        return
                with open(stderr_file, 'r') as f:
                    if stderr != f.read():
                        tests.failed.append(case)
                    else:
                        tests.passed.append(case)
                return
        elif expected.compile_pass:
            if proc.returncode != 0:
                tests.failed.append(case)
            else:
                subprocess.call("rm -f {}".format(exe).split(" "))
                tests.passed.append(case)
            return
        else:
            if proc.returncode != 0:
                tests.failed.append(case)
                return
        proc = subprocess.Popen(
            exe, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output = TestResult()
        (stdout, stderr) = proc.communicate()
        output.exit_code = proc.returncode
        output.stdout = stdout.decode() if stdout.decode() else None
        output.stderr = stderr.decode() if stderr.decode() else None
        subprocess.call("rm -f {}".format(exe).split(" "))
        if output != expected:
            print(output)
            print(expected)
            tests.failed.append(case)
        else:
            tests.passed.append(case)


def test_dir(dirname, options):
    for test in dirname.iterdir():
        run_test(test, options)


def collect_tests(case: pathlib.Path):
    if case.is_dir():
        n = 0
        for test in case.iterdir():
            n += collect_tests(test)
        return n

    if case.suffix in {'.o'}:
        subprocess.call("rm -f {}".format(case).split(" "))
        return 0

    if case.suffix in {'.stderr'}:
        return 0

    with open(case, 'r') as f:
        if dummy([f.readline()]):
            return 0

    return 1


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

    tests.collected_tests = collect_tests(test)
    run_test(test, args)
    print("\x1b[K", end='', flush=True)
    subprocess.call(
        "rm -f {}".format(" ".join(map(str, tests.units.values()))).split())
    tests.print_results()
