from ctypes import *
from gnatllvm import build_and_load, Func

(complex, ) = build_and_load(
    ['complex.adb'], 'absolute',
    Func('_ada_complex', argtypes=[c_int], restype=c_int),
)

assert complex(2) == 66
assert complex(7) == 77
assert complex(21) == 66
assert complex(201) == 66
assert complex(42) == 55
assert complex(40) == 44
