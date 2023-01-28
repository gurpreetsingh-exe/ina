from __future__ import annotations
from copy import deepcopy
from typing import Generator
from Token import *


def lexer_from_file(filepath: str) -> Lexer:
    with open(filepath, "r") as f:
        program = f.read()
        return lexer_from_src(program, filepath)


def lexer_from_src(src: str, filepath: str) -> Lexer:
    return Lexer(src, filepath)


class Lexer:
    def __init__(self, program: str, filepath: str):
        self.program = program
        self.loc = Loc(0, 0, filepath)
        self.curr_char = self.program[self.loc.offset]

    def advance(self):
        self.loc.offset += 1
        if self.curr_char == '\n':
            self.loc.col = 0
            self.loc.line += 1
        else:
            self.loc.col += 1
        self.curr_char = self.program[self.loc.offset] if self.loc.offset < len(
            self.program) else None

    def lex_word(self, method) -> str:
        buffer = ''

        while self.curr_char and method(self) and (not self.curr_char.isspace()):
            buffer += self.curr_char
            self.advance()

        return buffer

    def lexfile(self) -> Generator:
        while self.curr_char:
            loc = deepcopy(self.loc)

            if self.curr_char == None:
                yield Token(self.curr_char, loc)
                break

            if self.curr_char.isspace():
                self.advance()
                continue

            elif self.curr_char.isdigit():
                word = self.lex_word(
                    lambda self: self.curr_char.isdigit() or self.curr_char == ".")
                loc.len = self.loc.offset - loc.offset
                try:
                    int(word)
                    typ = TokenKind.Int
                except ValueError:
                    float(word)
                    typ = TokenKind.Float

                yield Token(typ, loc)

            elif self.curr_char == "\"":
                buf = ""
                self.advance()
                while self.curr_char != "\"":
                    buf += self.curr_char
                    self.advance()
                self.advance()
                loc.len = self.loc.offset - loc.offset
                yield Token(TokenKind.Str, loc)

            elif self.curr_char.isalpha() or self.curr_char == "_":
                word = self.lex_word(
                    lambda self: self.curr_char.isalnum() or self.curr_char == "_")
                loc.len = self.loc.offset - loc.offset

                if word in Keywords:
                    yield Token(Keywords[word], loc)
                elif word in {'true', 'false'}:
                    yield Token(TokenKind.Bool, loc)
                else:
                    yield Token(TokenKind.Ident, loc)

            elif self.curr_char in Punctuators:
                prev = self.curr_char
                self.advance()
                compound = prev + self.curr_char

                if compound == "//":
                    while self.curr_char != "\n":
                        self.advance()
                    continue
                elif compound == "..":
                    self.advance()
                    if self.curr_char == ".":
                        loc.len = self.loc.offset - loc.offset
                        yield Token(Punctuators["..."], loc)
                        self.advance()
                    else:
                        assert False, "Unexpected token"
                elif compound in Punctuators:
                    loc.len = self.loc.offset - loc.offset
                    yield Token(Punctuators[compound], loc)
                    self.advance()
                else:
                    loc.len = self.loc.offset - loc.offset
                    yield Token(Punctuators[prev], loc)

            else:
                assert False, f"unreachable {self.curr_char}"
