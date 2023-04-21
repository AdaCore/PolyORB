#!/usr/bin/env python
"""
%prog [OPTIONS] [TEST_PATH]

Run the PolyORB testsuite.

Current directory must be the testsuite source directory.
Rlimit must be on the PATH.

To run only core tests:
    %prog core/

To run a single example:
    %prog examples/corba-all_functions/ALL_FUNCTIONS_4/test.py

See %prog -h for more help.
"""

from e3.fs import ls
from e3.os.fs import which
from e3.os.process import Run

from e3.testsuite import Testsuite
from e3.testsuite.testcase_finder import ParsedTest, TestFinder
from e3.testsuite.driver.adacore import AdaCoreLegacyTestDriver
from e3.testsuite.driver.classic import ClassicTestDriver, TestAbortWithError
from e3.testsuite.control import AdaCoreLegacyTestControlCreator
from e3.testsuite.result import TestStatus, FailureReason

import logging
import os
import re
import sys
import time

logger = logging.getLogger('polyorb.testsuite')

class PolyORBRegTestFinder(TestFinder):
    """Looks for testcases in dirctories which have a test script.

    All regtest directories hold a test.sh or test.cmd file.
    """

    def __init__(self, driver_cls) -> None:
        """Initialize a PolyORBTestFinder.

        :param driver_cl: TestDriver subclass to use for all tests that are found.
        """
        self.driver_cls = driver_cls

    def probe(
        self,
        testsuite,
        dirpath: str,
        dirnames,
        filenames,
    ):
        """Return a test if the "dirpath" directory contains a testcase.

        All the regtest are run using a test.sh or a test.cmd file. This is used to
        find the testcases.

        :param testsuite: Testsuite instance that is looking for testcases.
        :param dirpath: Directory to probe for a testcase.
        :param dirnames: List of directories that "dirpath" contains.
        :param filenames: List of files that "dirpath" contains.
        """

        # Skip reg-testsuite dir
        if 'reg-testsuite' in dirpath:
            return None

        command_file_names = ["test.sh", "test.cmd"]
        test_name = os.path.basename(dirpath)

        # If the directory hold any known command files then it is a test
        if any(cmd_file in filenames for cmd_file in command_file_names):
            return ParsedTest(test_name, self.driver_cls, {}, dirpath)
        else:
            return None


class PolyORBTestFinder(TestFinder):
    """Looks for testcases in dirctories which have a test script.

    All testcase directories hold a test.py file.
    """
    def __init__(self, driver_cls) -> None:
        """Initialize a PolyORBTestFinder.

        :param driver_cls: TestDriver subclass to use for all tests that are found.
        """
        self.driver_cls = driver_cls

    def probe(
        self,
        testsuite,
        dirpath: str,
        dirnames,
        filenames,
    ):
        """Return a test if the "dirpath" directory contains a testcase.

        All the test are runned using a test.py file. This is used to
        find the testcases.

        :param testsuite: Testsuite instance that is looking for testcases.
        :param dirpath: Directory to probe for a testcase.
        :param dirnames: List of directories that "dirpath" contains.
        :param filenames: List of files that "dirpath" contains.
        """
        test_name = os.path.basename(dirpath)

         # If the directory hold 'test.py' then it is a test
        if 'test.py' in filenames:
            return ParsedTest(test_name, self.driver_cls, {}, dirpath)
        else:
            return None

class PolyORBDiffTestDriver(AdaCoreLegacyTestDriver):
    """Run a test based on diff.

    It is used to run the regtests and it is heavily based on the
    AdacoreLegacyTestDriver from e3-testsuite.
    """
    def set_up(self):
        """Setup the test environment."""
        super().set_up()

        # RLIMIT is used to define the test's subprocesses timeout.
        self.test_environ["RLIMIT"] = self.test_control.opt_results["RLIMIT"]

class PolyORBClassicTestDriver(ClassicTestDriver):
    """Run a test from the main testsuite"""

    # Save the test process for analyze
    test_process = None

    @property
    def test_control_creator(self):
        """Set the control creator.

        The AdaCore legacy test control reads the test.opt to extract the test
        configuration.
        """
        return AdaCoreLegacyTestControlCreator(self.env.discs)

    def set_up(self):
        """Run initialization operations before a test runs.

        Prepare the test environement.
        """
        super().set_up()

        # RLIMIT is used to define the test's subprocesses timeout.
        self.test_env["RLIMIT"] = self.test_control.opt_results["RLIMIT"]

        self.test_env["TEST_NAME"] = self.test_name
        self.test_env.update(self.env.test_environ)

        self.test_process = None


    def run(self):
        """Execute the command inside the tests and compares the result.

        The tests usaly run two subprocesses. To allow them to timeout, the test
        timeout is set to twice the RLIMIT.
        """
        cmd = ["python", "test.py"]
        start_time = time.time()
        self.test_process = self.shell(
            cmd,
            cwd=self.test_env["working_dir"],
            env=self.test_env,
            timeout=int(2.1 * int(self.test_env["RLIMIT"])),
            catch_error=False,
        )
        self.result.time = time.time() - start_time

    def compute_failures(self):
        result = []
        if self.test_process.status != 0:

            # List of error messages which never appears in an actual test
            error_msgs = (
                "is not defined",)
            if any(msg in self.output.log for msg in error_msgs):
                raise TestAbortWithError(
                    "The test fail is likly to be du to a testsuite error."
                )

        # Findout if the test failed
        if (
            self.test_process is not None and
            self.process_may_have_timed_out(self.test_process)
        ):
            self.result.failure_reasons.add(FailureReason.TIMEOUT)
            result.append("timeout")
        elif "TEST FAILED" in self.output.log:

            # The test has written "TEST FAILED" in the output so it is a failure for
            # sure.
            self.result.failure_reasons.add(FailureReason.DIFF)
            result.append("TEST FAILED")
        elif "Traceback" in self.output.log:

            # The python code of the test raised exepection.
            # This is a failure but not necessarily a testsuite failure. It might be
            # expected by a XFAIL test.
            self.result.failure_reasons.add(FailureReason.DIFF)
            result.append("Test run failed")
        elif not re.search(rf"{self.test_name} PASSED", self.output.log):

            # The test has not properly failed but still the test did not explicitly
            # passed.
            self.result.failure_reasons.add(FailureReason.DIFF)
            result.append("Test is not passed")

        return result

class PolyORBTestsuite(Testsuite):

    test_driver_map = {"RegTestDriver": PolyORBDiffTestDriver, "TestDriver": PolyORBClassicTestDriver}
    default_driver = "RegTestDriver"

    @property
    def test_finders(self):
        """Return test finders to probe tests directories."""
        return [
            PolyORBTestFinder(PolyORBClassicTestDriver),
            PolyORBRegTestFinder(PolyORBDiffTestDriver),
        ]

    def set_up(self):
        """Create the output directory in which the results are stored."""
        # Add current directory in PYTHONPATH (to find test_utils.py)
        self.env.add_search_path('PYTHONPATH', os.path.join(os.getcwd(), 'tests'))
        fixed_support_dir = os.path.join(os.getcwd(), 'fixed_support_dir')
        self.env.add_search_path('FIXED_SUPPORT_DIR', fixed_support_dir)
        self.env.add_path(os.path.join(fixed_support_dir))
        self.env.add_path('.')  # many tests expect '.' in the PATH

        # Avoid extra debug traces
        os.environ['POLYORB_LOG_DEFAULT'] = 'error'

        # Generate the discs list for test.opt parsing
        # Always add 'ALL'
        common_discs = self.env.discriminants

        # Be backward compatible with the old IDL tests
        # Set the polyorb discriminant and export the IDLCOMP
        # environment variable.
        common_discs.append('PolyORB')
        common_discs.append('PolyORB_IAC')
        os.environ['IDLCOMP'] = 'iac'

        # Retrieve also the polyorb specific discriminants
        p = Run(
            [which('bash'), which('polyorb-config').replace('\\', '/'), '--config',]
        )

        # First find the support application perso.
        match = re.search('Application *personalities *: (.+)', p.out)
        if (match is not None):
            common_discs += ['app_%s' % k for k in match.group(1).split()]

        # Then the supported protocols
        match = re.search('Protocol *personalities *: (.+)', p.out)
        if (match is not None):
            common_discs += ['proto_%s' % k for k in match.group(1).split()]

        # Then the supported services
        match = re.search('Services *: (.+)', p.out)
        if (match is not None):
            common_discs += ['serv_%s' % k for k in match.group(1).split()]

        # Do we have ssl support ?
        if re.search('SSL *support *: *yes', p.out):
            common_discs.append('ssl_support')

        # Expand ~ and ~user contructions for user PATH
        if self.env.options.build_dir is None:
            self.env.options.build_dir = os.path.join(os.getcwd(), os.pardir)
        else:
            self.env.options.build_dir = os.path.expanduser(self.env.options.build_dir)

        # Save the discriminants
        with open(self.env.options.output_dir + '/discs', 'w') as f_disk:
            f_disk.write(", ".join(common_discs))

        self.env.discs = common_discs

        if self.env.options.testsuite_src_dir is None:
            self.env.options.testsuite_src_dir = os.path.join(os.getcwd())
        else:
            self.env.options.testsuite_src_dir = os.path.expanduser(
                self.env.options.testsuite_src_dir)

        os.environ['TEST_CONFIG'] = os.path.join(os.getcwd(), 'env.dump')
        self.env.log_dir = os.path.join(os.getcwd(), 'log')
        self.env.store(os.environ['TEST_CONFIG'])

        self.env.test_environ = dict(os.environ)
        self.env.test_environ["BUILD_DIR"] = self.env.options.build_dir
        self.env.test_environ["SRC_DIR"] = self.env.options.testsuite_src_dir
        self.env.test_environ["LOG_DIR"] = self.env.log_dir
        self.env.test_environ["COVERAGE"] = str(self.env.options.coverage)
        self.env.test_environ["VERBOSE"] = str(self.env.options.verbose)

    def add_options(self, parser):
        """Add testsuite specific switches."""
        parser.add_argument(
            '-b', '--build-dir',
            dest = 'build_dir',
            help="Old result dir (to generate the report)",
        )

        parser.add_argument(
            '--testsuite-src-dir',
            dest='testsuite_src_dir',
            help='path to polyorb testsuite sources',
        )

        parser.add_argument(
            '--coverage',
            dest='coverage',
            action='store_true',
            default=False, help='generate coverage information',
        )


if __name__ == "__main__":
    sys.exit(PolyORBTestsuite().testsuite_main())
