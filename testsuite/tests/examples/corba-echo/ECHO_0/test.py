
from test_utils import *
import sys

if not client_server(r'../examples/corba/echo/client', r'',
                     r'../examples/corba/echo/server', r''):
    fail()

