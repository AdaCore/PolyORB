
from test_utils import *
import sys

if not client_server(r'../examples/corba/echo/dynclient', r'soap.conf',
                     r'../examples/corba/echo/server', r'soap.conf'):
    fail()

