from ctypes import *
from gnatllvm import build_and_load, Func

(pgcd, ) = build_and_load(
    ['pgcd.adb'], 'pgcd',
    Func('_ada_pgcd', argtypes=[c_uint, c_uint], restype=c_uint),
)

def py_pgcd(a, b):
    while True:
        c = a % b
        if c == 0:
            return b
        a = b
        b = c

def test_pgcd(a, b):
    result = py_pgcd(a, b)
    assert pgcd(a, b) == result

test_pgcd(1, 1)
test_pgcd(1, 2)
test_pgcd(2, 2)
test_pgcd(6, 8)
test_pgcd(345 ** 2, 765 ** 2)
