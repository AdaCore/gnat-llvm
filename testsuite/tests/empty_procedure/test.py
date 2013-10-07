from ctypes import *
from gnatllvm import build_and_load, Func

(empty_procedure, ) = build_and_load(
    ['empty_procedure.adb'], 'empty_procedure',
    Func('empty_procedure', argtypes=[], restype=None),
)

empty_procedure()
