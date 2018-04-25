"""
Test that FOR loops have a constant memory consumption.
"""

from ctypes import *
from gnatllvm import build_and_load, Func

(Foo, ) = build_and_load(
    ['foo.adb'], 'foo',
    Func('_ada_foo', argtypes=[c_uint], restype=c_bool),
)

assert Foo(100)
