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
        run_test = run_test.replace('test.py', '')
        return [t for t in test_list if run_test.rstrip('/') in t]

if __name__ == "__main__":
    main()
