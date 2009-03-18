
from test_utils import *
import sys

if not client_server(r'../examples/corba/echo/dynclient',
                     r'../examples/corba/echo/server'):
    sys.exit(1)

