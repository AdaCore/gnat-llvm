from ctypes import *
from gnatllvm import build_and_load, Func, Structure

class Stack_Element_Record(Structure):
   pass
Stack_Element = POINTER(Stack_Element_Record)

class Stack(Structure):
   _field_ = [
      ('First_Stack_El', Stack_Element),
   ]

Stack_Element_Record._fields_ = [
   ('el', c_uint),
   ('next', Stack_Element),
]

Stack_Access = POINTER(Stack)

(Init, Append, Pop, ) = build_and_load(
    ['stack.adb'], 'stack',
    Func('stack__init_stack', argtypes=[Stack_Access], restype=None),
    Func('stack__append', argtypes=[Stack_Access], restype=None),
    Func('stack__pop', argtypes=[Stack_Access], restype=c_uint),
)


S = Stack()
Init(byref(S))

for i in range(1, 5):
   print 'Append ({})'.format(i)
   Append(byref(S), i)

for _ in range(1, 5):
   print 'Pop () = {}'.format(Pop(byref(S)))
