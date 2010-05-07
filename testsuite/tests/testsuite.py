#!/usr/bin/env gnatpython
"""
%prog [OPTIONS] [TEST_PATH]

Run the PolyORB testsuite

To run only core tests:
    %prog core/
To run a single example:
    %prog examples/corba-all_functions/ALL_FUNCTIONS_4/test.py

See %prog -h for more help.
"""

from gnatpython.env import Env
from gnatpython.ex import Run, STDOUT
from gnatpython.fileutils import mkdir, rm
from gnatpython.main import Main
from gnatpython.mainloop import (MainLoop, add_mainloop_options,
                                 generate_collect_result,
                                 generate_run_testcase)
from gnatpython.testdriver import add_run_test_options
from gnatpython.reports import ReportDiff

from glob import glob

import logging
import os
import sys

DEFAULT_TIMEOUT = 60

logger = logging.getLogger('polyorb.testsuite')


def main():
    """Run the testsuite and generate reports"""
    # Parse the command lines options
    m = Main(add_targets_options=True)
    add_mainloop_options(m)
    add_run_test_options(m)
    m.add_option('--diffs', dest='diffs', action='store_true',
                 default=False, help='show diffs on stdout')
    m.add_option("--old-result-dir", type="string", default=None,
                 help="Old result dir (to generate the report)")
    m.add_option('-b', '--build-dir', dest='build_dir',
                 help='separate PolyORB build directory')
    m.add_option('--testsuite-src-dir', dest='testsuite_src_dir',
                 help='path to polyorb testsuite sources')
    m.add_option('--coverage', dest='coverage', action='store_true',
                 default=False, help='generate coverage information')
    m.parse_args()

    # Various files needed or created by the testsuite
    results_file = m.options.output_dir + '/results'
    report_file = m.options.output_dir + '/report'

    if not m.options.failed_only:
        rm(m.options.output_dir, True)
        mkdir(m.options.output_dir)

    # Add current directory in PYTHONPATH (to find test_utils.py)
    env = Env()
    env.add_search_path('PYTHONPATH', os.getcwd())

    # Generate the discs list for test.opt parsing
    # Always add 'ALL'
    common_discs = Env().discriminants
    with open(m.options.output_dir + '/discs', 'w') as f_disk:
        f_disk.write(", ".join(common_discs))

    # Expand ~ and ~user contructions for user PATH
    if m.options.build_dir is None:
        m.options.build_dir = os.path.join(os.getcwd(), os.pardir, os.pardir)
    else:
        m.options.build_dir = os.path.expanduser(m.options.build_dir)

    if m.options.testsuite_src_dir is None:
        m.options.testsuite_src_dir = os.path.join(os.getcwd(), os.pardir)
    else:
        m.options.testsuite_src_dir = os.path.expanduser(
            m.options.testsuite_src_dir)

    # Compute the test list
    if m.args:
        test_glob = m.args[0]
    else:
        test_glob = None
    test_list = filter_list('./*/*/*/test.py', test_glob)

    collect_result = generate_collect_result(
        m.options.output_dir, results_file, m.options.diffs)
    run_testcase = generate_run_testcase('run-test.py',
                                         common_discs, m.options)

    os.environ['TEST_CONFIG'] = os.path.join(os.getcwd(), 'env.dump')
    env.options = m.options
    env.log_dir = os.path.join(os.getcwd(), 'log')
    env.store(os.environ['TEST_CONFIG'])
    MainLoop(test_list, run_testcase, collect_result, m.options.mainloop_jobs)

             #gen_run_testcase(options.build_dir, options.testsuite_src_dir,
             #                 options.coverage),

    # Generate the report file
    ReportDiff(m.options.output_dir,
               m.options.old_result_dir).txt_image(report_file)


def filter_list(pattern, run_test=""):
    """Compute the list of test matching pattern

    If run_test is not null, run only tests containing run_test
    """
    test_list = [os.path.dirname(p) for p in glob(pattern)]
    if not run_test:
        test_list.append("always_fail")
        return test_list
    else:
        return [t for t in test_list if run_test.rstrip('/') in t]


def gen_run_testcase(build_dir, testsuite_src_dir, coverage):
    """Returns the run_testcase function"""

    # Set build_dir variable to the root of the build area, so test_utils.py
    # can find it. This should be the directory in which PolyORB's ./configure
    # was run. If the --build-dir option was specified, use that; otherwise,
    # default to the source area (i.e. polyorb directory, two levels up from
    # here).

    # Set testsuite_src_dir variable to the root of the testsuite area. Default
    # to one level up from here.
    if build_dir is None:
        build_dir = os.path.join(os.getcwd(), os.pardir, os.pardir)

    if testsuite_src_dir is None:
        testsuite_src_dir = os.path.join(os.getcwd(), os.pardir)

    def run_testcase(test, _job_info):
        """Run a single test

        If limit is not set, run rlimit with DEFAULT_TIMEOUT
        """
        logger.debug("Running " + test.testdir)
        timeout = test.getopt('limit')
        if timeout is None:
            timeout = DEFAULT_TIMEOUT

        mkdir(os.path.dirname(os.path.join('output', test.filename)))

        return Run([sys.executable,
                    test.filename,
                    '--timeout', str(timeout),
                    '--out-file', os.path.join('output', test.filename),
                    '--testsuite-src-dir', os.path.realpath(testsuite_src_dir),
                    '--build-dir', os.path.realpath(build_dir),
                    '--coverage=' + str(coverage)],
                   bg=True, output=os.path.join('output',
                                                test.filename + '.error'),
                   error=STDOUT, timeout=int(timeout) + DEFAULT_TIMEOUT)
    return run_testcase

if __name__ == "__main__":
    main()
