from ctypes import *
from gnatllvm import build_and_load, Func, Structure

class Signed_Number(Structure):
    _fields_ = [
        ('Positive', c_bool),
        ('N', c_uint),
    ]

(Create, Add ) = build_and_load(
    ['signed_numbers.adb'], 'signed_numbers',
    Func('signed_numbers__create',
        argtypes=[c_uint, c_bool],
        restype=Signed_Number),
    Func('signed_numbers__add',
        argtypes=[Signed_Number, Signed_Number],
        restype=Signed_Number),
)

N1, N2 = Create(1, True), Create(3, False)

print 'N1 = ', N1
print 'N2 = ', N2
print 'N1 + N2', Add(N1, N2)
