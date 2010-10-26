#!/usr/bin/env python

import os
import re
import sys

from gnatpython.fileutils import mkdir, cd

CLIENT_SERVER_TEMPLATE = """
from test_utils import *
import sys

if not client_server(r'%(client_cmd)s', r'%(client_conf)s',
                     r'%(server_cmd)s', r'%(server_conf)s'):
    sys.exit(1)

"""

LOCAL_TEMPLATE = """
from test_utils import *
import sys

if not local(r'%(command)s', r'%(conf)s'):
    sys.exit(1)

"""

SCENARIO_SECTION = 'scenario'
TEST_SECTION     = 'test'
CLIENT_SECTION   = 'client'
SERVER_SECTION   = 'server'

def parse_scenario(filename):
    """Parse a scenario file and create the corresponding test directories"""
    scenario = open(filename)

    test_dict = {}
    current_section = SCENARIO_SECTION
    current_test = ""

    for line in scenario:
        if line.startswith('['):
            test_name = re.match(r'\[(.*) (.*)\]', line)
            if test_name:
                current_section = test_name.group(1)
                current_test = test_name.group(2)
                if current_section == TEST_SECTION:
                    test_dict[current_test] = {}

        elif current_section != SCENARIO_SECTION:
            # Do not parse scenario section.
            line_def = re.match(r'(.*)=(.*)', line)
            if line_def:
                left  = line_def.group(1)
                right = line_def.group(2)
                if not current_section in test_dict[current_test]:
                    test_dict[current_test][current_section] = {}
                test_dict[current_test][current_section][left] = right

    scenario.close()
    full_name = os.path.basename(filename)
    sep = full_name.find('-')

    parent_dir = full_name[:sep]
    mkdir(parent_dir)

    scenario_dir = full_name[sep + 1:-5]
    mkdir(os.path.join(parent_dir, scenario_dir))
    cd(os.path.join(parent_dir, scenario_dir))

    for test_name in test_dict:
        test_type = test_dict[test_name][TEST_SECTION]['type']
        if test_type == 'client_server':
            mkdir(test_name)
            f = open(os.path.join(test_name, 'test.py'), 'w')
            f.write(CLIENT_SERVER_TEMPLATE %
                    {'client_cmd' :
                     test_dict[test_name][CLIENT_SECTION]['command'],
                     'client_conf' :
                     test_dict[test_name][CLIENT_SECTION].get('config_file',
                                                              ''),
                     'server_cmd' :
                     test_dict[test_name][SERVER_SECTION]['command'],
                     'server_conf' :
                     test_dict[test_name][SERVER_SECTION].get('config_file',
                                                              ''),
                    })
            f.close()

            if 'expected_failure' in test_dict[test_name][TEST_SECTION]:
                f = open(os.path.join(test_name, 'test.opt'), 'w')
                f.write('ALL XFAIL\n')
                f.close()

        elif test_type == 'local':
            mkdir(test_name)
            f = open(os.path.join(test_name, 'test.py'), 'w')
            f.write(LOCAL_TEMPLATE %
                    {'command' :
                     test_dict[test_name][TEST_SECTION]['command'],
                     'conf' :
                     test_dict[test_name][TEST_SECTION].get('config_file', '')
                    })
            f.close()

            if 'expected_failure' in test_dict[test_name][TEST_SECTION]:
                f = open(os.path.join(test_name, 'test.opt'), 'w')
                f.write('ALL XFAIL\n')
                f.close()
        else:
            print 'unknown type for test: ' + test_name

if __name__ == "__main__":
    parse_scenario(sys.argv[1])
