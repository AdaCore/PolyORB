
from test_utils import *
import sys

if not client_server(r'../examples/corba/all_functions/dynclient', r'scenarios/polyorb_conf/soap.conf',
                     r'../examples/corba/all_functions/server', r'scenarios/polyorb_conf/soap.conf'):
    fail()

