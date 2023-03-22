from __future__ import annotations
from ..Ast import *
from ..front.Parser import Parser
from ..front.Lexer import lexer_from_file
import pathlib


def search_mod(name) -> pathlib.Path:
    mod_path = pathlib.Path(name)
    if mod_path.exists():
        return mod_path
    mod_path = pathlib.Path(f"library/{name}")
    if mod_path.exists():
        return mod_path
    raise ImportError(f"`{mod_path.stem}` not found")


def parse_file(filepath: pathlib.Path) -> Module:
    lexer = lexer_from_file(str(filepath))
    tokens = list(lexer.lexfile())
    mod = Parser(lexer.program, tokens).parse()
    # try:
    #     ImportResolver(mod).resolve()
    # except RecursionError:
    #     print(f"recursive imports in {filepath}")
    #     exit(1)
    return mod


class ImportResolver:
    def __init__(self, mod: Module) -> None:
        self.mod = mod

    def resolve(self):
        items = []
        for item in self.mod:
            match item:
                case Import(name):
                    mod = search_mod(name + ".ray")
                    items += parse_file(mod.absolute()).items
        self.mod.items += items
        self.mod.items = [
            item for item in self.mod.items if not isinstance(item, Import)]
