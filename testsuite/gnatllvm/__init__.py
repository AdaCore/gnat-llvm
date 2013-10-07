from collections import namedtuple
from ctypes import cdll
import os
import os.path
import subprocess
import sys

from gnatpython.fileutils import mkdir, rm


Func = namedtuple('Func', 'name argtypes restype')


def change_ext(filename, new_ext):
    """Replace "filename" extension with "new_ext"."""
    return '{}{}{}'.format(
        os.path.splitext(filename)[0],
        os.path.extsep,
        new_ext
    )


def get_shared_func(shared, name, argtypes=[], restype=None):
    """Get a function from a shared object.

    Return the "name" function from the "shared" shared object. Also specify
    argument types and result type.
    """
    func = getattr(shared, name)
    func.argtypes = argtypes
    func.restype = restype
    return func


def gnat_to_bc(adb):
    """Compile the "adb" unit and return the result bitcode filename."""
    subprocess.check_call(['llvm-gnatcompile', '-c', adb])
    return change_ext(adb, 'bc')

def gnat_to_obj(adb):
    """Compile the "adb" unit and return the result object file filename."""
    subprocess.check_call(['llc', gnat_to_bc(adb)])
    subprocess.check_call(['gcc', '-c', change_ext(adb, 's')])
    return change_ext(adb, 'o')

def gnat_to_shared(adb_list, name):
    """Compile sources to a shared object.

    Compile the set of unit "adb_list" and link them to a dynamic library
    "name". Return the shared object filename.
    """
    obj_list = [gnat_to_obj(adb) for adb in adb_list]
    result = change_ext(name, 'so')
    subprocess.check_call(['gcc', '-shared', '-o', result] + obj_list)
    return result

def build_and_load(adb_list, name, *objects):
    """Build a shared object from sources and load objects from it.

    Compile the set of unit "adb_list" and link them to a dynamic library
    "name". Then, load it using ctypes and return the sequence of loaded
    "objects".
    """
    shared = gnat_to_shared(adb_list, name)
    lib = cdll.LoadLibrary(shared)
    result = []

    for obj in objects:
        if isinstance(obj, Func):
            loaded_obj = get_shared_func(
                lib, obj.name, obj.argtypes, obj.restype)
        else:
            raise TypeError('Invalid object type: {}'.format(type(obj)))
        result.append(loaded_obj)

    return result
