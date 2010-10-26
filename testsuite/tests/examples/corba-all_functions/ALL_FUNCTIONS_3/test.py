
from test_utils import *
import sys

if not client_server(r'../examples/corba/all_functions/dynclient', r'soap.conf',
                     r'../examples/corba/all_functions/server', r'soap.conf'):
    fail()

