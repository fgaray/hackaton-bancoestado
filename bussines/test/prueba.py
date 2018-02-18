try:
    import haskell
    HASKELL = True
except:
    HASKELL = False

def sum(x, y):
    return x + y


def checkHaskellModule(x):
    return str(dir(haskell))

def callHaskell(x, y):
    if HASKELL:
        haskell.call("printHaskell")
        print dir(haskell)
        return x + y
    else:
        return 0
