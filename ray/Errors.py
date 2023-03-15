from sys import stderr
from .Token import Span
from .utils import File
from .Ast import Ty


class Error:
    def __init__(self, msg: str) -> None:
        self.msg = msg
        self.span: Span | None = None

    def emit(self, _: File, exit=False):
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

    def emit(self, file: File, _exit=False):
        emit(self, file, ": variable redefinition\n")
        if _exit:
            exit(1)


class TypesMismatchError(Error):
    def __init__(self, msg: str) -> None:
        super().__init__(msg)

    def emit(self, file: File, _exit=False):
        emit(self, file, ": types mismatch\n")
        if _exit:
            exit(1)


class NotFound(Error):
    def __init__(self, name: str, msg: str) -> None:
        super().__init__(msg)
        self.name = name

    def emit(self, file: File, _exit=False):
        emit(self, file, f": `{self.name}` not found\n")
        if _exit:
            exit(1)


class CastError(Error):
    def __init__(self, msg: str) -> None:
        super().__init__(msg)

    def emit(self, file: File, _exit=False):
        emit(self, file, f": cast error\n")
        if _exit:
            exit(1)


class DerefError(Error):
    def __init__(self, ty: Ty) -> None:
        super().__init__("")
        self.ty = ty

    def emit(self, file: File, _exit=False):
        emit(self, file, f": `{self.ty}` cannot be dereferenced\n")
        if _exit:
            exit(1)


class MissingStructFieldError(Error):
    def __init__(self, msg: str, missing_field, struct) -> None:
        super().__init__(msg)
        self.missing_field = missing_field
        self.struct = struct

    def emit(self, file: File, _exit=False):
        emit(
            self, file, f": missing field `{self.missing_field}` in `{self.struct}`\n")
        if _exit:
            exit(1)


class MissingTyAnnError(Error):
    def __init__(self, msg: str) -> None:
        super().__init__(msg)

    def emit(self, file: File, _exit=False):
        emit(
            self, file, f": missing type annotation\n")
        if _exit:
            exit(1)


def c(format: str) -> str:
    return f"\033[30;1m{format}\033[0m"


def b(format: str) -> str:
    return f"\033[1m{format}\033[0m"


def err(format: str) -> str:
    return f"\033[1;31m{format}\033[0m"


def ok(format: str) -> str:
    return f"\033[1;32m{format}\033[0m"
