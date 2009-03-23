#!/usr/bin/env gnatpython

"""./testsuite.py [OPTIONS] [TEST_PATH]

Run the PolyORB testsuite

To run only core tests:
    ./testsuite.py core/
To run a single example:
    ./testsuite.py examples/corba-all_functions/ALL_FUNCTIONS_4/test.py

See ./testsuite.py -h for more help.
"""

from gnatpython.arch import Arch
from gnatpython.env import Env
from gnatpython.ex import Run, PIPE, STDOUT
from gnatpython.main import Main
from gnatpython.mainloop import MainLoop
from gnatpython.optfileparser import OptFileParse
from gnatpython.report import Report, GenerateRep

from glob import glob

import logging
import os
import sys

DEFAULT_TIMEOUT = 60

def main():
    """Run the testsuite and generate reports"""
    # Parse the command lines options
    m, target = __parse_options()

    # Generate the discs list for test.opt parsing
    # Always add 'ALL'
    common_discs = ['ALL', target.platform]

    # Compute the test list
    non_dead_list, dead_list = generate_testcase_list(
        filter_list('./*/*/*/test.py',
                    m.options.run_test),
        common_discs)

    # Add current directory in PYTHONPATH (to find test_utils.py)
    env = Env()
    env.add_search_path('PYTHONPATH', os.getcwd())

    # Main loop :
    #   - run all the tests
    #   - collect the test results
    #   - generate the res file
    report = Report('res_polyorb')

    # First report all dead tests
    for test in dead_list:
        report.add(test.filename, 'DEAD')

    # Then run all non dead tests
    MainLoop(non_dead_list,
             gen_run_testcase(m.options.build_dir),
             gen_collect_result(report, m.options.diffs), 
             m.options.jobs)
    report.write()

    # Human readable report (rep file)
    rep = GenerateRep('res_polyorb')
    report_file = open('rep_polyorb', 'w')
    report_file.write(rep.get_subject())
    report_file.write(rep.get_report()) 
    report_file.close()

def filter_list(pattern, run_test=""):
    """Compute the list of test matching pattern

    If run_test is not null, run only tests containing run_test
    """
    test_list = glob(pattern)
    if not run_test:
        return test_list
    else:
        return [t for t in test_list if run_test in t]

def generate_testcase_list(test_list, discs):
    """Generate the testcase list

    Returns two sorted list:
        - the non dead test list (to be run in the mainloop)
        - the dead test list (not to be run)
    """
    dead_list = []
    non_dead_list = []
    for test in test_list:
        tc = TestCase(test)
        tc.parseopt(discs)
        if tc.is_dead():
            dead_list.append(tc)
        else:
            non_dead_list.append(tc)

    # Sort lists
    non_dead_list.sort()
    dead_list.sort()
    return (non_dead_list, dead_list)

class TestCase(object):
    """Creates a TestCase object.

    Contains the result fo the test.opt parsing
    """
    def __init__(self, filename):
        """Create a new TestCase for the given filename"""
        self.testdir      = os.path.dirname(filename)
        self.filename     = filename
        self.expected_out = None
        self.opt          = None

    def __lt__(self, right):
        """Use filename alphabetical order"""
        return self.filename < right.filename

    def parseopt(self, tags):
        """Parse the test.opt with the given tags"""
        test_opt = os.path.join(self.testdir, 'test.opt')
        if os.path.exists(test_opt):
            self.opt = OptFileParse(tags, test_opt)
        self.expected_out = self.getopt('out', 'test.out')

    def getopt(self, key, default=None):
        """Get the value extracted from test.opt that correspond to key

        If key is not found. Returns default.
        """
        if self.opt is None:
            return default
        else:
            return self.opt.get_value(key, default_value=default)

    def is_dead(self):
        """Returns True if the test is DEAD"""
        if self.opt is None:
            return False
        else:
            return self.opt.is_dead

def gen_run_testcase(build_dir):
    """Returns the run_testcase function"""

    # Set build_dir variable to the root of the build area, so test_utils.py
    # can find it. This should be the directory in which PolyORB's ./configure
    # was run. If the --build-dir option was specified, use that; otherwise,
    # default to the source area (i.e. polyorb directory, two levels up from
    # here).
    polyorb_sources_dir = os.path.join(os.pardir, os.pardir)
    if build_dir is None:
        build_dir = os.path.join(os.getcwd(), polyorb_sources_dir)
    else:
        build_dir = os.path.realpath(build_dir)

    def run_testcase(test, _job_info):
        """Run a single test

        If limit is not set, run rlimit with DEFAULT_TIMEOUT
        """
        logging.debug("Running " + test.testdir)
        timeout = test.getopt('limit')
        if timeout is None:
            timeout = DEFAULT_TIMEOUT

        return Run([sys.executable,
                    os.path.join(test.filename),
                    '--timeout', str(timeout),
                    '--build-dir', os.path.realpath(build_dir)],
                   bg=True, output=PIPE, error=STDOUT)
    return run_testcase

def gen_collect_result(report, show_diffs=False):
    """Returns the collect_result function"""
    # success - xfail status dict
    status_dict = {True: {True: 'UOK', False: 'OK'},
                   False: {True: 'XFAIL', False: 'FAILED'}}

    def collect_result(test, process, _job_info):
        """Collect a test result"""
        xfail = test.getopt('xfail', None) is not None
        success = process.status == 0

        status = status_dict[success][xfail]
        logging.info("%-60s %s" % (test.filename, status))
        if not success:
            report.add(test.filename, status, diff=process.out)
            if show_diffs:
                logging.info(process.out)
        else:
            report.add(test.filename, status)

    return collect_result

def __parse_options():
    """Parse command lines options"""
    m = Main()
    m.add_option('--diffs', dest='diffs', action='store_true',
                 default=False, help='show diffs on stdout')
    m.add_option('-j', '--jobs', dest='jobs', type='int',
                 metavar='N', default=1, help='Allow N jobs at once')
    m.add_option('-b', '--build-dir', dest='build_dir',
                  help='separate PolyORB build directory')
    m.parse_args()

    if m.args:
        # Run only one test
        m.options.run_test = os.path.sep + m.args[0]
        logging.info("Running only test '%s'" % m.options.run_test)
    else:
        m.options.run_test = ""

    target = Arch()
    return (m, target)

if __name__ == "__main__":
    main()
