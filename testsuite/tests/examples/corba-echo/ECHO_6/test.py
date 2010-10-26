
from test_utils import *
import sys

if not client_server(r'../examples/corba/echo/client', r'giop_1_1.conf',
                     r'../examples/corba/echo/server', r'giop_1_1.conf'):
    fail()

