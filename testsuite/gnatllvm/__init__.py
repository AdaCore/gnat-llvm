from collections import namedtuple
import ctypes
from ctypes import cdll
import os
import os.path
import subprocess
import sys

from gnatpython.fileutils import mkdir, rm, which


TESTSUITE_ROOT = os.environ['TESTSUITE_ROOT']

GNATLLVM_GPR = os.path.join(TESTSUITE_ROOT, '..', 'gnat_llvm.gpr')
SRCCOV_DIR = os.path.join(TESTSUITE_ROOT, 'srccov')
UNITS_LIST = os.path.join(SRCCOV_DIR, 'units.list')
TRACES_DIR = os.path.join(SRCCOV_DIR, 'traces')

USE_NATIVE_GNAT = bool(os.environ.get('USE_NATIVE_GNAT', False))
SOURCE_COVERAGE_LEVEL = os.environ.get('SOURCE_COVERAGE_LEVEL', None)

LLVM_GNATCOMPILE = which('llvm-gcc')
TESTCASE_NAME = os.environ['TESTCASE_NAME']

Func = namedtuple('Func', 'name argtypes restype')


trace_count = 0

def get_trace_filename():
    global trace_count
    trace_count += 1
    test_basename = TESTCASE_NAME.lstrip('./').replace('/', '-')
    return '{}-{}.trace'.format(test_basename, trace_count)

def get_library_name(filename):
    """Return the library file name for the given Ada source file name."""
    return os.path.splitext(filename)[0]

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


def gnat_to_bc(adb, gargs=None):
    """Compile the "adb" unit and return the result bitcode filename."""

    if USE_NATIVE_GNAT:
        raise RuntimeError(
            'Cannot produce LLVM bitcode with the native GNAT compiler')

    prefix = (
        [] if not SOURCE_COVERAGE_LEVEL else
        [
            'gnatcov', 'run',
            '-o', os.path.join(TRACES_DIR, get_trace_filename()),
            '-c', SOURCE_COVERAGE_LEVEL,
            '-P{}'.format(GNATLLVM_GPR),
            '--units=@{}'.format(UNITS_LIST),
            '-eargs',
        ]
    )
    gargs = list(gargs) if gargs else []
    subprocess.check_call(prefix + [LLVM_GNATCOMPILE, '-c', adb] + gargs)
    return change_ext(adb, 'bc')

def gnat_to_obj(adb, gargs=None):
    """Compile the "adb" unit and return the result object file filename."""

    if USE_NATIVE_GNAT:
        gargs = list(gargs) if gargs else []
        subprocess.check_call(['gcc', '-c', '-fPIC', '-g'] + gargs + [adb])
    else:
        subprocess.check_call(
            ['llc', '-relocation-model=pic', gnat_to_bc(adb, gargs)])
        subprocess.check_call(['gcc', '-c', change_ext(adb, 's')])
    return change_ext(adb, 'o')

def gnat_to_shared(adb_list, name, gargs=None):
    """Compile sources to a shared object.

    Compile the set of unit "adb_list" and link them to a dynamic library
    "name". Return the shared object filename.
    """
    obj_list = [gnat_to_obj(adb, gargs) for adb in adb_list]
    result = change_ext(name, 'so')

    if USE_NATIVE_GNAT:
        library_list = [get_library_name(adb) for adb in adb_list]
        binder_output = 'b~{}.adb'.format(library_list[-1])
        subprocess.check_call(
            ['gnatbind', '-L{}'.format(name), '-shared'] + library_list)
        obj_list.append(gnat_to_obj(binder_output, gargs))
        extra_options = ['-lgnat']
    else:
        extra_options = []

    subprocess.check_call(
        ['gcc', '-shared', '-o', result] + extra_options + obj_list)
    return result

def build_and_load(adb_list, name, *objects, **kwargs):
    """Build a shared object from sources and load objects from it.

    Compile the set of unit "adb_list" and link them to a dynamic library
    "name". Then, load it using ctypes and return the sequence of loaded
    "objects".
    """
    shared = gnat_to_shared(adb_list, name, **kwargs)
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


class Structure(ctypes.Structure):

    def __str__(self):
        return '{} ({})'.format(type(self).__name__, ', '.join(
            '{}={}'.format(field_name, getattr(self, field_name))
            for field_name, field_type in self._fields_
        ))
