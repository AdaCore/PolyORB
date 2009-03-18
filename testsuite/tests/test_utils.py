#!/usr/bin/env gnatpython

"""test utils

This module is imported by all testcase. It parse the command lines options
and provide some usefull functions.

You should never call this module directly. To run a single testcase, use
 ./testsuite.py NAME_OF_TESTCASE
"""

from gnatpython.ex import Run, PIPE, STDOUT
from gnatpython.main import Main

import expect # this is in gnatpython only
import os
import re

# The POLYORB_BUILD_DIR environment variable was set (by testsuite.py) to point
# to the build area, which may or may not be the same as the polyorb source
# area. Test executables are found within the build area.
PWD = os.getcwd ()
SRC_DIR = os.path.normpath (os.path.join (PWD, os.pardir, os.pardir))
BUILD_DIR = os.path.normpath (os.environ ['POLYORB_BUILD_DIR'])
BASE_DIR = os.path.normpath (os.path.join (PWD.replace (SRC_DIR, BUILD_DIR), os.pardir))

def client_server(client_cmd, server_cmd):
    """Run a client server testcase

    Run server_cmd and extract the IOR string.
    Run client_cmd with the server IOR string
    Check for "END TESTS................   PASSED"
    if found return True
    """
    client = os.path.join(BASE_DIR, client_cmd)
    server = os.path.join(BASE_DIR, server_cmd)

    # Run the server command and retrieve the IOR string
    server_pid = expect.non_blocking_spawn(server, [])
    if not server_pid:
        print "Error when running " + server
        return False

    result = expect.expect (server_pid, [r"IOR:([a-z0-9]+)['|\n]"], 2.0)
    if result != 0:
        print "Expect error"
        expect.close(server_pid)
        return False

    IOR_str = expect.expect_out (server_pid, 2)

    # Run the client with the IOR argument
    p = Run([client, IOR_str], output=PIPE, error=STDOUT,
            timeout=options.timeout)

    # Kill the server process
    expect.close(server_pid)

    if re.search(r"END TESTS.*PASSED", p.out):
        print p.out
        return True
    else:
        print p.out
        return False

def local(cmd):
    """Run a local test

    Execute the give command.
    Check for "END TESTS................   PASSED"
    if found return True
    """
    command = os.path.join(BASE_DIR, cmd)
    p = Run([command], output=PIPE, error=STDOUT,
            timeout=options.timeout)

    if re.search(r"END TESTS.*PASSED", p.out):
        print p.out
        return True
    else:
        print p.out
        return False

def parse_cmd_line():
    """Parse command line

    Returns options object
    """
    main = Main(require_docstring=False)
    main.add_option('--timeout', dest='timeout', type=int,
                    default=None)
    main.parse_args()
    return main.options

# Parse command lines options
options = parse_cmd_line()

