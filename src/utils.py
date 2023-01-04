errors = []

def panic(msg):
    errors.append(msg + "\n")
    assert False, msg

