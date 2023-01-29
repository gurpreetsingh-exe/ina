from enum import Enum, auto


class TokenKind(Enum):
    Int = auto()
    Float = auto()
    Bool = auto()
    Str = auto()

    Intrinsic = auto()
    Ident = auto()

    Mut = auto()
    Imm = auto()
    Let = auto()
    Extern = auto()
    Fn = auto()
    If = auto()
    Else = auto()
    As = auto()
    Loop = auto()

    PLUS = auto()  # `+`
    MINUS = auto()  # `-`
    STAR = auto()  # `*`
    SLASH = auto()  # `/`
    EQ = auto()  # `=`
    SEMI = auto()  # `;`
    LT = auto()  # `<`
    GT = auto()  # `>`
    COLON = auto()  # `:`
    LPAREN = auto()  # `(`
    RPAREN = auto()  # `)`
    LCURLY = auto()  # `{`
    RCURLY = auto()  # `}`
    LBRACKET = auto()  # `[`
    RBRACKET = auto()  # `]`
    COMMA = auto()  # `,`
    DOUBLEQUOTE = auto()  # `"`
    POUND = auto()  # `#`
    AT = auto()  # `@`
    AMPERSAND = auto()  # `&`
    PIPE = auto()  # `|`
    TILDE = auto()  # `~`
    BANG = auto()  # `!`
    PERCENT = auto()  # `%`
    DOT = auto()  # `.`

    LT2 = auto()  # `<<`
    GT2 = auto()  # `>>`
    PIPE2 = auto()  # `||`
    AMPERSAND2 = auto()  # `&&`
    EQ2 = auto()  # `==`
    BANGEQ = auto()  # `!=`
    ARROW = auto()  # `->`

    DOT3 = auto()  # `...`

    EOF = auto()
    UNDEFINED = auto()


Keywords = {
    "mut": TokenKind.Mut,
    "imm": TokenKind.Imm,
    "extern": TokenKind.Extern,
    "fn": TokenKind.Fn,
    "let": TokenKind.Let,
    "if": TokenKind.If,
    "else": TokenKind.Else,
    "as": TokenKind.As,
    "loop": TokenKind.Loop,
}

Punctuators = {
    '+': TokenKind.PLUS,
    '-': TokenKind.MINUS,
    '*': TokenKind.STAR,
    '/': TokenKind.SLASH,
    '=': TokenKind.EQ,
    ';': TokenKind.SEMI,
    '<': TokenKind.LT,
    '>': TokenKind.GT,
    ':': TokenKind.COLON,
    '(': TokenKind.LPAREN,
    ')': TokenKind.RPAREN,
    '{': TokenKind.LCURLY,
    '}': TokenKind.RCURLY,
    '[': TokenKind.LBRACKET,
    ']': TokenKind.RBRACKET,
    ',': TokenKind.COMMA,
    '"': TokenKind.DOUBLEQUOTE,
    '#': TokenKind.POUND,
    '@': TokenKind.AT,
    '&': TokenKind.AMPERSAND,
    '|': TokenKind.PIPE,
    '~': TokenKind.TILDE,
    '!': TokenKind.BANG,
    '%': TokenKind.PERCENT,
    '.': TokenKind.DOT,
    '<<': TokenKind.LT2,
    '>>': TokenKind.GT2,
    '&&': TokenKind.AMPERSAND2,
    '||': TokenKind.PIPE2,
    '==': TokenKind.EQ2,
    '!=': TokenKind.BANGEQ,
    '->': TokenKind.ARROW,
    '...': TokenKind.DOT3,
}


def get_token_name(ty):
    match ty:
        case TokenKind.Ident: return "identifier"
        case TokenKind.Int: return "int"
        case TokenKind.Float: return "float"
        case TokenKind.Bool: return "bool"
        case TokenKind.Str: return "str"
        case TokenKind.Intrinsic: return "intrinsic"
        case TokenKind.Extern: return "extern"
        case TokenKind.Fn: return "fn"
        case TokenKind.Mut: return "mut"
        case TokenKind.Imm: return "imm"
        case TokenKind.Let: return "let"
        case TokenKind.If: return "if"
        case TokenKind.Else: return "else"
        case TokenKind.As: return "as"
        case TokenKind.PLUS: return "+"
        case TokenKind.MINUS: return "-"
        case TokenKind.STAR: return "*"
        case TokenKind.SLASH: return "/"
        case TokenKind.EQ: return "="
        case TokenKind.SEMI: return ";"
        case TokenKind.LT: return "<"
        case TokenKind.GT: return ">"
        case TokenKind.COLON: return ":"
        case TokenKind.LPAREN: return "("
        case TokenKind.RPAREN: return ")"
        case TokenKind.LCURLY: return "{"
        case TokenKind.RCURLY: return "}"
        case TokenKind.LBRACKET: return "["
        case TokenKind.RBRACKET: return "]"
        case TokenKind.COMMA: return ","
        case TokenKind.DOUBLEQUOTE: return '"'
        case TokenKind.POUND: return "#"
        case TokenKind.AT: return "@"
        case TokenKind.AMPERSAND: return "&"
        case TokenKind.PIPE: return "|"
        case TokenKind.TILDE: return "~"
        case TokenKind.BANG: return "!"
        case TokenKind.PERCENT: return "%"
        case TokenKind.DOT: return "."
        case TokenKind.LT2: return "<<"
        case TokenKind.GT2: return ">>"
        case TokenKind.PIPE2: return "||"
        case TokenKind.AMPERSAND2: return "&&"
        case TokenKind.EQ2: return "=="
        case TokenKind.BANGEQ: return "!="
        case TokenKind.ARROW: return "->"
        case TokenKind.DOT3: return "..."


class Token:
    __match_args__ = ('kind', 'loc', )

    def __init__(self, kind, loc):
        self.kind = kind
        self.loc = loc

    def raw(self, src):
        loc = self.loc
        return src[loc.offset:loc.offset + loc.len]

    def __repr__(self):
        return f"{str(self.kind).ljust(16)}\t=> `{str(self.loc)}`"


class Loc:
    def __init__(self, offset, len, file=""):
        self.offset = offset
        self.line = 0
        self.col = 0
        self.len = len
        self.file = file

    def fmt(self):
        return f"{self.file}:{self.line + 1}:{self.col + 1}"

    def __repr__(self) -> str:
        return self.fmt()


class Span:
    def __init__(self, start, end):
        self.start = start
        self.end = end
