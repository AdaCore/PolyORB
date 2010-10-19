#!/usr/bin/env python

"""test utils

This module is imported by all testcase. It parse the command lines options
and provide some useful functions.

You should never call this module directly. To run a single testcase, use
 ./testsuite.py NAME_OF_TESTCASE
"""

from gnatpython.env import Env
from gnatpython.ex import Run, STDOUT
from gnatpython.fileutils import mkdir

from subprocess import Popen, PIPE

import os
import re
import sys

POLYORB_CONF = "POLYORB_CONF"

RLIMIT = int(os.environ['RLIMIT'])
TEST_NAME = os.environ['TEST_NAME']

# Restore testsuite environment
Env().restore(os.environ['TEST_CONFIG'])

# All executable tests path are relative to PolyORB testsuite dir
BASE_DIR = os.path.join(Env().options.build_dir, 'testsuite')
CONF_DIR = os.path.join(BASE_DIR, 'tests', 'confs')

EXE_EXT = Env().target.os.exeext

OUTPUT_FILENAME = os.path.join(Env().log_dir, TEST_NAME)

mkdir(os.path.dirname(OUTPUT_FILENAME))


def assert_exists(filename):
    """Assert that the given filename exists"""
    assert os.path.exists(filename), "%s not found" % filename


def client_server(client_cmd, client_conf, server_cmd, server_conf):
    """Run a client server testcase

    Run server_cmd and extract the IOR string.
    Run client_cmd with the server IOR string
    Check for "END TESTS................   PASSED"
    if found return True
    """
    print "Running client %s (config=%s)\nserver %s (config=%s)" % (
        client_cmd, client_conf, server_cmd, server_conf)
    client = os.path.join(BASE_DIR, client_cmd + EXE_EXT)
    server = os.path.join(BASE_DIR, server_cmd + EXE_EXT)

    # Check that files exist
    assert_exists(client)
    assert_exists(server)

    for conf_file in (server_conf, client_conf):
        if conf_file:
            assert_exists(os.path.join(CONF_DIR, conf_file))

    server_env = os.environ.copy()
    server_env[POLYORB_CONF] = server_conf

    try:
        # Run the server command and retrieve the IOR string
        server_handle = Popen(['rlimit', str(RLIMIT), server], stdout=PIPE, env=server_env)
        while True:
            line = server_handle.stdout.readline()
            if "IOR:" in line:
                IOR_str = re.match(r".*(IOR:[a-z0-9]+)['|\n\r]",
                                   line).groups()[0]
                break
        # Remove eol and '
        IOR_str = IOR_str.strip()
        print IOR_str

        # Run the client with the IOR argument

        if client_conf != server_conf:
            client_env = os.environ.copy()
            client_env[POLYORB_CONF] = client_conf
        else:
            client_env = None

        Run(make_run_cmd([client, IOR_str], Env().options.coverage),
            output=OUTPUT_FILENAME + 'server', error=STDOUT,
            timeout=RLIMIT, env=client_env)

        for elmt in [client, server]:
            if Env().options.coverage:
                run_coverage_analysis(elmt)

    except Exception, e:
        print e
    finally:
        server_handle.terminate()

    return _check_output(OUTPUT_FILENAME + 'server', 'server')


def local(cmd, config_file, args=None):
    """Run a local test

    Execute the given command.
    Check for "END TESTS................   PASSED"
    if found return True

    PARAMETERS:
        cmd: the command to execute
        config_file: to set POLYORB_CONF
        args: list of additional parameters
    """
    args = args or []
    print "Running %s %s (config=%s)" % (cmd, " ".join(args), config_file)
    if config_file:
        assert_exists(os.path.join(CONF_DIR, config_file))
    os.environ[POLYORB_CONF] = config_file

    command = os.path.join(BASE_DIR, cmd + EXE_EXT)
    assert_exists(command)
    Run(make_run_cmd([command] + args, Env().options.coverage),
        output=OUTPUT_FILENAME + 'local', error=STDOUT,
        timeout=RLIMIT)
    if Env().options.coverage:
        run_coverage_analysis(command)
    return _check_output(OUTPUT_FILENAME + 'local', 'local')


def _check_output(output_file, test_name):
    """Check that END TESTS....... PASSED is contained in the output"""
    if os.path.exists(output_file):
        test_outfile = open(output_file)
        test_out = test_outfile.read()
        test_outfile.close()

        if re.search(r"END TESTS.*PASSED", test_out):
            print "%s PASSED" % test_name
            return True
        else:
            print test_out
            return False


def make_run_cmd(cmd, coverage=False):
    """Create a command line for Run in function of coverage

    Returns command and arguments list
    """
    L = []
    if coverage:
        L.extend(['xcov', '--run', '--target=i386-linux', '-o',
                  cmd[0] + '.trace', cmd[0]])
        if len(cmd) > 1:
            L.append('-eargs')
            L.extend(cmd[1:])
    else:
        L.extend(cmd)
    return L


def run_coverage_analysis(command):
    """Run xcov with appropriate arguments to retrieve coverage information

    Returns an object of type run
    """
    return Run(['xcov', '--coverage=branch', '--annotate=report',
                command + ".trace"],
               output=OUTPUT_FILENAME + '.trace', error=STDOUT,
               timeout=RLIMIT)


def fail():
    print "TEST FAILED"
    sys.exit(1)
