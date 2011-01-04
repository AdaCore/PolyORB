
from test_utils import *
import sys

if not client_server(
        r'../examples/corba/all_types/dynclient',
        r'soap.conf',
        r'../examples/corba/all_types/server',
        r'soap.conf'):
    fail()

