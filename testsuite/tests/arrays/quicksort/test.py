from ctypes import *
from gnatllvm import build_and_load, Func

(driver, ) = build_and_load(
    ['quicksort.adb', 'driver.adb'], 'quicksort',
    Func('_ada_driver', argtypes=[], restype=c_bool),
)

print driver()
