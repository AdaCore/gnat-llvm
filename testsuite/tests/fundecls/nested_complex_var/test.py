from ctypes import *
from gnatllvm import build_and_load, Func

(Identity, ) = build_and_load(
    ['identity.adb'], 'identity',
    Func('_ada_identity', argtypes=[c_uint], restype=c_uint),
)

for n in (0, 1, 2, 10, 100, 1000):
    print 'Identity ({}) = {}'.format(n, Identity(n))
