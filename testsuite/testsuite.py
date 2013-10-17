#!/usr/bin/env python

"""./testsuite.py [OPTIONS] [TEST_PATH]

Run the GNAT-LLVM testsuite. See ./testsuite.py -h for help"""

from glob import glob
import itertools
import logging
import os
import sys

from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.fileutils import (
    echo_to_file, split_file)
from gnatpython.main import Main
from gnatpython.mainloop import (MainLoop, add_mainloop_options,
                                 generate_collect_result,
                                 generate_run_testcase,
                                 setup_result_dir)
from gnatpython.testdriver import add_run_test_options
from gnatpython.reports import ReportDiff


class Testsuite:

    def __init__(self):
        self.main =  Main()
        add_mainloop_options(self.main, extended_options=True)
        add_run_test_options(self.main)
        self.main.add_option(
            "--diffs", dest="view_diffs", action="store_true",
            default=False, help="show diffs on stdout")

    def run(self):
        """Run the testsuite"""

        self.main.parse_args()

        # Various files needed or created by the testsuite
        # creates :
        #   the ouput directory (out by default)
        #   the report file
        #   the results file

        setup_result_dir(self.main.options)

        test_list = self.get_test_list()

        env = Env()
        discs = [env.target.platform]

        if self.main.options.discs:
            discs += self.main.options.discs.split(',')

        collect_result = generate_collect_result(
            self.main.options.output_dir,
            self.main.options.results_file,
            self.main.options.view_diffs,
            use_basename=False)

        run_testcase = generate_run_testcase(
            'run_test.py', discs, self.main.options,
            use_basename=False)

        MainLoop(
            test_list, run_testcase, self.collect_result,
            self.main.options.mainloop_jobs)
        # Generate the report file
        diff = ReportDiff(
            self.main.options.output_dir,
            self.main.options.old_output_dir
        )
        diff.txt_image(self.main.options.report_file)


    def get_test_list(self):
        if self.main.args:
            return sorted(itertools.chain(
                *[self.iter_tests(t) for t in self.main.args]))
        else:
            return sorted(self.iter_tests('.'))


    def collect_result(self, name, process, job_info):

        def get_filename(ext):
            return os.path.join(
                self.main.options.output_dir,
                '{}{}{}'.format(os.path.normpath(name), os.path.extsep, ext))

        test_name = os.path.relpath(name, os.getcwd())

        test_result = split_file(
            get_filename('result'), ignore_errors=True)
        test_result = (
            test_result[0]
            if test_result else
            'CRASH:cannot read result file')

        test_status, test_msg = test_result.split(':', 1)

        echo_to_file(
            self.main.options.results_file,
            '{}:{}: {}\n'.format(test_name, test_status, test_msg),
            append=True)

        logging_func = (
            logging.error
            if test_status in ('DIFF', 'CRASH') else
            logging.info)

        logging_func('{:>5}  {}: {}'.format(
            test_status, test_name, test_msg))
        if self.main.options.view_diffs:
            try:
                with open(get_filename('diff'), 'r') as diff_file:
                    logging_func(diff_file.read().strip())
            except (OSError, IOError):
                pass


    def iter_tests(self, rootdir):
        """Generate directory names for all tests under "rootdir"."""
        for dirpath, dirs, files in os.walk(rootdir):
            if 'test.py' in files:
                yield dirpath


if __name__ == "__main__":
    if os.path.dirname(__file__):
        os.chdir(os.path.dirname(__file__))
    Testsuite().run()
