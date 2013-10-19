from ctypes import *
from gnatllvm import build_and_load, Func

(identity, ) = build_and_load(
    ['identity.adb'], 'identity',
    Func('_ada_identity', argtypes=[c_int], restype=c_int),
)

for i in (-1, 0, 1):
    print 'identity({}) = {}'.format(i, identity(i))
