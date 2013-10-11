#!/usr/bin/env python
"""Usage: run_test.py [options] test_dir

Run a test located in test_dir
"""

from gnatpython.env import Env
from gnatpython.fileutils import mkdir
from gnatpython.main import Main
from gnatpython.testdriver import TestRunner, add_run_test_options

from glob import glob
import os.path
import sys


TEST = sys.modules['__main__']
TESTDIR = os.path.dirname(os.path.abspath(TEST.__file__))
TEST_NAME = os.path.basename(TESTDIR)
os.chdir(TESTDIR)


def main():
    """Run a single test"""

    GNATLLVM_ROOT = os.path.dirname(os.getcwd())
    LLVM_PATHS = glob(os.path.join(
        GNATLLVM_ROOT, 'llvm-ada', 'llvm-*src', '*', 'bin'))

    env = Env()
    # Make support Python packages available to tests.
    env.add_search_path('PYTHONPATH', os.path.join(GNATLLVM_ROOT, 'testsuite'))
    # Make the GNAT-LLVM compiler available to tests.
    env.add_path(os.path.join(GNATLLVM_ROOT, 'llvm-backend', 'bin'))
    # Make available the correct version of LLVM to tests.
    for path in LLVM_PATHS:
        env.add_path(path)

    m = Main()
    add_run_test_options(m)
    m.parse_args()
    if not m.args:
        sys.exit("Error: 1 argument expected. See -h")

    if m.options.restricted_discs is not None:
        m.options.restricted_discs = m.options.restricted_discs.split(',')

    # Make sure the result directory exists.
    test_name = m.args[0]
    result_dir = os.path.dirname(os.path.join(
        m.options.output_dir, test_name.strip('/')))
    mkdir(result_dir)

    t = TestRunner(test_name,
                   m.options.discs,
                   result_dir,
                   m.options.tmp,
                   m.options.enable_cleanup,
                   m.options.restricted_discs,
                   len(m.args) > 1 and m.args[1:] or None,
                   m.options.failed_only)
    t.execute()


if __name__ == '__main__':
    main()
