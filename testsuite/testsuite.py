#!/usr/bin/env python

"""./testsuite.py [OPTIONS] [TEST_PATH]

Run the GNAT-LLVM testsuite. See ./testsuite.py -h for help"""

from collections import defaultdict
from glob import glob
import itertools
import logging
import os
import subprocess
import sys

import pygments
from pygments.formatters import get_all_formatters, get_formatter_by_name
from pygments.lexers import get_lexer_by_name
from pygments.style import Style
from pygments.styles import get_style_by_name
from pygments.token import Token

from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.fileutils import (
    echo_to_file, split_file,
    mkdir, rm)
from gnatpython.main import Main
from gnatpython.mainloop import (MainLoop, add_mainloop_options,
                                 generate_collect_result,
                                 generate_run_testcase,
                                 setup_result_dir)
from gnatpython.testdriver import add_run_test_options
from gnatpython.reports import ReportDiff


def add_path(env_var, path):
    """Add the given PATH to the ENV_VAR environment variable."""
    previous = os.environ.get(env_var, None)
    os.environ[env_var] = (
        '{}:{}'.format(path, previous)
        if previous else
        path
    )

def get_gnatls_paths(cmd='gnatls -v'):
    """Get source, object and project search paths from "gnatls -v" output."""
    source_spath = []
    object_spath = []
    project_spath = []

    gnatls_output, _ = subprocess.Popen(
        cmd, shell=True, stdout=subprocess.PIPE).communicate()

    current_spath = None
    for line in gnatls_output.split('\n'):
        try:
            current_spath = {
                'Source Search Path:': source_spath,
                'Object Search Path:': object_spath,
                'Project Search Path:': project_spath,
            }[line]
        except KeyError:
            pass
        else:
            continue

        if line.startswith('   '):
            line = line.lstrip()
            current_spath.append(
                os.getcwd() if line.startswith('<') else line
            )

    return (source_spath, object_spath, project_spath)


class Testsuite:

    def __init__(self):
        self.main =  Main()
        add_mainloop_options(self.main, extended_options=True)
        add_run_test_options(self.main)

        # Output formatting options
        self.main.add_option(
            '--diffs', dest='view_diffs', action='store_true',
            default=False, help='show diffs on stdout')
        self.main.add_option(
            '--formatter', dest='formatter',
            choices=sum([f.aliases for f in get_all_formatters()], []),
            default='terminal256',
            help='format for the output')

        # Tests running options
        self.main.add_option(
            '--native-gnat', dest='native_gnat',
            action='store_true', default=False,
            help='Run the tests with the native GNAT compiler instead of'
                 ' GNAT-LLVM')
        self.main.add_option(
            '--srccov-level', dest='source_coverage_level',
            default=None,
            choices=('stmt', 'stmt+decision', 'stmt+mcdc', 'stmt+uc_mcdc'),
            help='Procude execution traces from tests and compute the source'
                 ' coverage with GNATcoverage at the requested level')

        # Mapping: test status -> count of tests that have this status
        self.summary = defaultdict(lambda: 0)

    def run(self):
        """Run the testsuite"""

        self.main.parse_args()

        os.environ['TESTSUITE_ROOT'] = os.getcwd()

        if self.main.options.native_gnat:
            os.environ['USE_NATIVE_GNAT'] = 'TRUE'

            # This is needed by the linker/ctypes in order to find the GNAT
            # runtime.
            _, object_spath, _ = get_gnatls_paths()
            for env_var in ('LIBRARY_PATH', 'LD_LIBRARY_PATH'):
                for path in object_spath:
                    add_path(env_var, path)

        if self.main.options.source_coverage_level:
            os.environ['SOURCE_COVERAGE_LEVEL'] = (
                self.main.options.source_coverage_level)

        # Various files needed or created by the testsuite
        # creates:
        #   the ouput directory (out by default)
        #   the report file
        #   the results file
        # For source coverage assessment:
        #   the trace directory
        #   the source coverage report directory

        setup_result_dir(self.main.options)
        if self.main.options.source_coverage_level:
            src_dir = os.path.join(os.getcwd(), '..', 'llvm-interface')
            srccov_dir = 'srccov'
            traces_dir = os.path.join(srccov_dir, 'traces')
            units_list = os.path.join(srccov_dir, 'units.list')
            for output_dir in (srccov_dir, traces_dir):
                rm(output_dir, recursive=True)
                mkdir(output_dir)

            # Prepare the list of units of interest
            units = set()
            for filename in os.listdir(src_dir):
                if filename.endswith('.adb') or filename.endswith('.ads'):
                    basename, ext = os.path.splitext(filename)
                    units.add(basename.replace('-', '.'))
            echo_to_file(
                units_list,
                '\n'.join(units)
            )

        self.formatter = get_formatter_by_name(
            self.main.options.formatter,
            style=OutputStyle)
        self.diff_lexer = get_lexer_by_name('diff')

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
        self.log(logging.info, self.format_summary())

        # Generate the source coverage report
        if self.main.options.source_coverage_level:
            gnatllvm_gpr = os.path.join(os.getcwd(), '..', 'gnat_llvm.gpr')

            traces_list = os.path.join(srccov_dir, 'traces.list')
            with open(traces_list, 'w') as f:
                for filename in os.listdir(traces_dir):
                    f.write('{}\n'.format(os.path.join(traces_dir, filename)))

            subprocess.check_call([
                'gnatcov', 'coverage',
                '--output-dir={}'.format(os.path.join(srccov_dir)),
                '-c', self.main.options.source_coverage_level,
                '-a', 'dhtml',
                '-P{}'.format(gnatllvm_gpr),
                '--units=@{}'.format(units_list),
                '@{}'.format(traces_list)
            ])


    def get_test_list(self):
        if self.main.args:
            return sorted(itertools.chain(
                *[self.iter_tests(t) for t in self.main.args]))
        else:
            return sorted(self.iter_tests('.'))

    def format_status(self, status):
        status_token = getattr(TestStatus, status, Token.Error)
        return (status_token, status)

    def format_summary(self):
        frame_start = [(Token.Punctuation, '=='), (Token.Text, ' ')]
        frame_stop = [(Token.Text, ' '), (Token.Punctuation, '==')]
        if self.summary:
            test_count = sum(n for n in self.summary.values())
            result = frame_start + [
                (Token.Number, str(test_count)),
                (Token.Text, ' tests executed:'),
            ] + frame_stop + [(Token.Text, '\n')]
            for status in sorted(self.summary.keys()):
                result.extend([
                    (Token.Text, ' ' * 4),
                    (Token.Number, str(self.summary[status]).ljust(5)),
                    (Token.Text, ' '),
                    self.format_status(status),
                    (Token.Text, '\n'),
                ])
        else:
            result = (
                frame_start +
                [(Token.Text, 'No test executed')] +
                frame_stop)
        return result

    def format_line(self, status, name, message):
        status_padding = max(0, 5 - len(status))
        result = []

        if status_padding:
            result.append((Token.Text, ' ' * status_padding))
        result.extend([
            self.format_status(status),
            (Token.Text, '  '),
            (Token.Text, name),
        ])
        if message:
            result.extend([
                (Token.Punctuation, ':'),
                (Token.Text, ' '),
                (Token.Comment, message)
            ])
        return result


    def format_diff(self, content):
        indent_token = (Token.Text, ' ' * 4)

        # Because of logging prefixes, skip the first line to avoid
        # misalignment.
        yield (Token.Text, '\n')
        is_empty_line = True

        tokens = pygments.lex(content, self.diff_lexer)
        # Prepend each line with an indentation, just to make the output
        # clearer.
        for ttype, value in tokens:
            for subval in value.split('\n'):
                if is_empty_line:
                    yield indent_token
                if subval:
                    yield (ttype, subval)
                    is_empty_line = False
                else:
                    yield (ttype, '\n')
                    is_empty_line = True
                    continue


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

        self.summary[test_status] += 1

        echo_to_file(
            self.main.options.results_file,
            '{}:{}: {}\n'.format(test_name, test_status, test_msg),
            append=True)

        logging_func = (
            logging.error
            if test_status in ('DIFF', 'CRASH') else
            logging.info)

        self.log(logging_func,
            self.format_line (test_status, test_name, test_msg))

        if self.main.options.view_diffs:
            try:
                with open(get_filename('diff'), 'r') as diff_file:
                    self.log(
                        logging_func,
                        self.format_diff(diff_file.read().strip()))
            except (OSError, IOError):
                pass


    def log(self, logging_func, tokens):
        logging_func(pygments.format(tokens, self.formatter))


    def iter_tests(self, rootdir):
        """Generate directory names for all tests under "rootdir"."""
        for dirpath, dirs, files in os.walk(rootdir):
            if 'test.py' in files:
                yield dirpath


def updated(dict1, dict2):
    """Return a copy of dict1 updated using dict2."""
    result = dict1.copy()
    result.update(dict2)
    return result

TestStatus = Token.TestStatus

class OutputStyle(Style):
    _native = get_style_by_name('native').styles

    default_style = ''
    styles = updated(_native, {
        TestStatus.DIFF: '#ed9d13',
        TestStatus.CRASH: 'bold #f00',
        TestStatus.XFAIL: '#3677a9',
        TestStatus.UOK: 'bold #447fcf',
        TestStatus.OK: '#6ab825',

        Token.Generic.Subheading: '#40ffff',
    })


if __name__ == "__main__":
    if os.path.dirname(__file__):
        os.chdir(os.path.dirname(__file__))
    Testsuite().run()
