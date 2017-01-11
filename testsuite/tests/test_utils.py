#!/usr/bin/env python

"""test utils

This module is imported by all testcase. It parse the command lines options
and provide some useful functions.

You should never call this module directly. To run a single testcase, use
 ./testsuite.py NAME_OF_TESTCASE
"""

from gnatpython.env import Env
from gnatpython.ex import Run, STDOUT, which
from gnatpython.fileutils import FileUtilsError, mkdir

from subprocess import Popen, PIPE
from time import sleep

import os
import re
import sys

import config

POLYORB_CONF = "POLYORB_CONF"

RLIMIT = int(os.environ['RLIMIT'])
TEST_NAME = os.environ['TEST_NAME']

# Restore testsuite environment
Env().restore(os.environ['TEST_CONFIG'])

# If POLYORB_TEST_VERBOSE is set to true, then output more data
VERBOSE = Env().options.verbose  # set by testsuite.py

# Main testsuite source dir
SRC_DIR = Env().options.testsuite_src_dir

# All executable tests path are relative to PolyORB testsuite build dir
BASE_DIR = os.path.join(Env().options.build_dir, 'testsuite')

# Shared configuration files are in tests/conf
CONF_DIR = os.path.join(SRC_DIR, 'tests', 'confs')

EXE_EXT = Env().target.os.exeext

OUTPUT_FILENAME = os.path.join(Env().log_dir, TEST_NAME)

USE_INSTALLED = config.use_installed == "yes"

try:
    if not os.path.isdir(os.path.dirname(OUTPUT_FILENAME)):
        mkdir(os.path.dirname(OUTPUT_FILENAME))
except FileUtilsError:
    # Ignore errors, multiple tests can be run in parallel
    pass


def assert_exists(filename):
    """Assert that the given filename exists"""
    assert os.path.exists(filename), "%s not found" % filename


def terminate(handle):
    """Terminate safely a process spawned using Popen"""

    if sys.platform.startswith('win'):
        try:
            handle.terminate()
        except WindowsError:
            # We got a WindowsError exception. This might occurs when we try to
            # terminate a process that is already dead. In that case we check
            # if the process is still alive. If yes we reraise the exception.
            # Otherwise we ignore it.
            is_alive = True
            for index in (1, 2, 3):
                is_alive = handle.poll() is None
                if not is_alive:
                    break
                sleep(0.1)
            if is_alive:
                # Process is still not terminated so reraise the exception
                raise
    else:
        handle.terminate()

def get_conf_path(conf_filename):
    if not conf_filename:
        return None

    if os.path.isabs(conf_filename):
        assert_exists(conf_filename)
        return conf_filename

    for d in (CONF_DIR, os.path.join(SRC_DIR, TEST_NAME)):
        p = os.path.join (d, conf_filename)
        if os.path.exists(p):
            return p

    assert False, "%s not found" % (conf_filename)

def get_tool_path(tool_dir, tool_name):
    """Return tool_dir and tool_name joined, unless we are using an installed
    version of PolyORB, in which case locate it on PATH."""

    if USE_INSTALLED:
        tool_path = which(tool_name)
        if tool_path == "":
            raise Exception('failed to locate %s on PATH' % tool_name)
        return tool_path
    else:
        return os.path.join(tool_dir, tool_name)

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

    server_env = os.environ.copy()
    if server_conf:
        server_env[POLYORB_CONF] = get_conf_path(server_conf)

    try:
        # Run the server command and retrieve the IOR string
        p_cmd_server = ['rlimit', str(RLIMIT), server]
        server_output_name = OUTPUT_FILENAME + '.server'
        server_handle = Popen(p_cmd_server,
	                      stdout=open(server_output_name, "wb"),
			      env=server_env)
        if server_conf:
            print 'RUN server: POLYORB_CONF=%s %s' % \
                (server_env[POLYORB_CONF], " ".join(p_cmd_server))
        else:
            print 'RUN server: %s' % " ".join(p_cmd_server)
	server_out = open(server_output_name, "r")
        while server_handle.returncode == None:
	    # Loop on readline() until we have a complete line
	    line = ""

	    while server_handle.returncode == None:
                server_handle.poll()
                line = line + server_out.readline()
                if len(line) > 0 and line[-1] in "\n\r":
                    break
                sleep(0.1)

            if "IOR:" in line:
	        try:
                  IOR_str = re.match(r".*(IOR:[a-z0-9]+)['|\n\r]",
                                   line).groups()[0]
                  break
		except:
		  print "Malformed IOR line <<%s>>" % (line)
		  raise

        if server_handle.returncode != None:
            print "server died"
            return

        server_out.close()
        # Remove eol and '
        IOR_str = IOR_str.strip()
        print IOR_str

        # Run the client with the IOR argument
        p_cmd_client = [client, IOR_str]

        if client_conf:
            client_env = os.environ.copy()
            client_env[POLYORB_CONF] = get_conf_path(client_conf)
            print 'RUN client: POLYORB_CONF=%s %s' % \
                (client_env[POLYORB_CONF], " ".join(p_cmd_client))
        else:
            client_env = None
            print "RUN client: %s" % " ".join(p_cmd_client)

        Run(make_run_cmd([client, IOR_str], Env().options.coverage),
            output=OUTPUT_FILENAME + '.client', error=STDOUT,
            timeout=RLIMIT, env=client_env)

        for elmt in [client, server]:
            if Env().options.coverage:
                run_coverage_analysis(elmt)

    except Exception, e:
        print e
    finally:
        terminate(server_handle)

    return _check_output(OUTPUT_FILENAME + '.client')


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

    p_cmd = [command] + args

    if VERBOSE:
        if config_file:
            print 'RUN: POLYORB_CONF=%s %s' % (config_file, " ".join(p_cmd))
        else:
            print 'RUN: %s' % " ".join(p_cmd)

    Run(make_run_cmd(p_cmd, Env().options.coverage),
        output=OUTPUT_FILENAME + 'local', error=STDOUT,
        timeout=RLIMIT)
    if Env().options.coverage:
        run_coverage_analysis(command)
    return _check_output(OUTPUT_FILENAME + 'local')


def _check_output(output_file):
    """Check that END TESTS....... PASSED is contained in the output"""
    if os.path.exists(output_file):
        test_outfile = open(output_file)
        test_out = test_outfile.read()
        test_outfile.close()

        if re.search(r"END TESTS.*PASSED", test_out):
            print "%s PASSED" % TEST_NAME
            return True
        else:
            print "%s FAILED" % TEST_NAME
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
