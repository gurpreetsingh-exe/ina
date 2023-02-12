from sys import stderr
from Token import Span
from utils import File
from Ast import Ty


class Error:
    def __init__(self, msg: str) -> None:
        self.msg = msg
        self.span: Span | None = None

    def emit(self, _: File):
        raise NotImplemented()


def emit(self, file: File, msg):
    assert self.span.start != None
    line_no = self.span.start.line
    # span_len = self.span.end.offset - self.span.start.offset
    span_len = self.span.end.offset + self.span.end.len - self.span.start.offset
    line_col = self.span.start.col
    lines = file.src.split('\n')
    line = lines[line_no]
    stderr.write(err("error"))
    stderr.write(b(msg))
    stderr.write(f"  {self.span.start}\n")
    line_col_buf = len(str(line_no + 1))
    a = " " * line_col_buf
    stderr.write(c(a))
    stderr.write(c("   ┃"))
    stderr.write("\n")
    a += "   ┃"
    stderr.write(c(f"  {line_no + 1} ┃"))
    stderr.write(f" {line}\n")
    buf = " " * (line_col + 1)
    stderr.write(f"{c(a)}{buf}")
    stderr.write(ok("^" * span_len))
    stderr.write(err(f" {self.msg}\n\n"))


class Redefinition(Error):
    def __init__(self, msg: str) -> None:
        super().__init__(msg)

    def emit(self, file: File):
        emit(self, file, ": variable redefinition\n")


class TypesMismatchError(Error):
    def __init__(self, msg: str) -> None:
        super().__init__(msg)

    def emit(self, file: File):
        emit(self, file, ": types mismatch\n")


class NotFound(Error):
    def __init__(self, name: str, msg: str) -> None:
        super().__init__(msg)
        self.name = name

    def emit(self, file: File):
        emit(self, file, f": {self.name} not found\n")


class CastError(Error):
    def __init__(self, msg: str) -> None:
        super().__init__(msg)

    def emit(self, file: File):
        emit(self, file, f": cast error\n")


class DerefError(Error):
    def __init__(self, ty: Ty) -> None:
        super().__init__("")
        self.ty = ty

    def emit(self, file: File):
        emit(self, file, f": `{self.ty}` cannot be dereferenced\n")


def c(format: str) -> str:
    return f"\033[30;1m{format}\033[0m"


def b(format: str) -> str:
    return f"\033[1m{format}\033[0m"


def err(format: str) -> str:
    return f"\033[1;31m{format}\033[0m"


def ok(format: str) -> str:
    return f"\033[1;32m{format}\033[0m"
