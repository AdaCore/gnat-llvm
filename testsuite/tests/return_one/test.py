from ctypes import *

from gnatllvm import (
    gnat_to_shared,
    get_shared_func
)

shared = gnat_to_shared(['return_one.adb'], 'return_one')
lib = cdll.LoadLibrary(shared)

return_one = get_shared_func(lib, 'return_one', restype=c_int)

assert return_one() == 1
