#!/usr/bin/env python

"""./testsuite.py [OPTIONS] [TEST_PATH]

Run the GNAT-LLVM testsuite. See ./testsuite.py -h for help"""

from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.main import Main
from gnatpython.mainloop import (MainLoop, add_mainloop_options,
                                 generate_collect_result,
                                 generate_run_testcase,
                                 setup_result_dir)
from gnatpython.testdriver import add_run_test_options
from gnatpython.reports import ReportDiff
from glob import glob
import os
import sys


def main():
    """Run the testsuite"""

    m = Main()
    add_mainloop_options(m, extended_options=True)
    add_run_test_options(m)
    m.add_option("--diffs", dest="view_diffs", action="store_true",
                 default=False, help="show diffs on stdout")
    m.parse_args()

    # Various files needed or created by the testsuite
    # creates :
    #   the ouput directory (out by default)
    #   the report file
    #   the results file

    setup_result_dir(m.options)

    if m.args:
        test_list = [t.strip('/') for t in m.args]
    else:
        test_list = sorted(iter_tests('.'))

    env = Env()

    # Make support Python packages available to tests.
    env.add_search_path("PYTHONPATH", os.getcwd())
    env.add_path(os.path.join(
        os.getcwd(), '..', 'llvm-backend', 'bin'))
    discs = [env.target.platform]

    if m.options.discs:
        discs += m.options.discs.split(',')

    collect_result = generate_collect_result(
        m.options.output_dir, m.options.results_file, m.options.view_diffs)

    run_testcase = generate_run_testcase('run_test.py', discs, m.options)

    MainLoop(test_list, run_testcase, collect_result, m.options.mainloop_jobs)
    # Generate the report file
    ReportDiff(m.options.output_dir,
               m.options.old_output_dir).txt_image(m.options.report_file)


def run_testcase(test, job_info):
    return Run(
        [sys.executable, os.path.join(test, 'test.py')],
        bg=True, output=None)


def iter_tests(rootdir):
    """Generate directory names for all tests under "rootdir"."""
    for dirpath, dirs, files in os.walk(rootdir):
        if 'test.py' in files:
            yield dirpath


if __name__ == "__main__":
    if os.path.dirname(__file__):
        os.chdir(os.path.dirname(__file__))
    main()
