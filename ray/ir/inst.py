from ..Ast import Ty


class Value:
    pass


class InstId:
    def __init__(self, i: int) -> None:
        self._i = i

    @property
    def i(self) -> int:
        return self._i

    def __repr__(self) -> str:
        return f"%{self._i}"


class Inst(Value):
    _id = 0

    def __init__(self, name=None) -> None:
        if not name:
            self.inst_id = Inst._id
            Inst._id += 1
        self.name = name

    @classmethod
    def reset(cls):
        cls._id = 0

    def __str__(self) -> str:
        return "%{}".format(self.inst_id if not self.name else self.name)


class Alloc(Inst):
    def __init__(self, ty: Ty, off: int) -> None:
        super().__init__()
        self.ty = ty
        self.off = off

    def __str__(self) -> str:
        return "    {} = alloc {}".format(super().__str__(), self.ty)


class Store(Inst):
    def __init__(self, dst: Inst, src: Value) -> None:
        super().__init__()
        self.dst = dst
        self.src = src

    def __str__(self) -> str:
        return "    store {}, {}".format(self.dst, self.src)


class Nop(Inst):
    def __init__(self) -> None:
        super().__init__()

    def __str__(self) -> str:
        return "    nop"
