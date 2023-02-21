errors = []

def panic(msg):
    errors.append(msg + "\n")
    assert False, msg

class File:
    def __init__(self, name, src) -> None:
        self.name = name
        self.src = src

