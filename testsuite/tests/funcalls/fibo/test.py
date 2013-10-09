from ctypes import *
from gnatllvm import build_and_load, Func

(fibo, ) = build_and_load(
    ['fibo.adb'], 'fibo',
    Func('fibo', argtypes=[c_int], restype=c_int),
)


def fib(n):
    return n if n < 2 else (fib(n - 1) + fib(n - 2))

assert fibo(0) == fib(0)
assert fibo(1) == fib(1)
assert fibo(4) == fib(4)
assert fibo(7) == fib(7)
