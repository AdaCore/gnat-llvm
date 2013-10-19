from ctypes import *
from gnatllvm import build_and_load, Func

(count_ones, ) = build_and_load(
    ['count_ones.adb'], 'count_ones',
    Func('_ada_count_ones', argtypes=[c_uint], restype=c_uint),
)

def check(n):
   expected_count = sum(
      (1 if digit == '1' else 0)
      for digit in str(n)
   )
   count = count_ones(n)
   assert count == expected_count

for n in (0, 1, 2, 3, 10, 11, 12, 50, 110, 111, 112):
   check(n)
