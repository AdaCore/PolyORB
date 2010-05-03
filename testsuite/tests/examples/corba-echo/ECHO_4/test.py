
from test_utils import *
import sys

if not client_server(r'../examples/corba/echo/dynclient', r'',
                     r'../examples/corba/echo/dynserver', r''):
    fail()

