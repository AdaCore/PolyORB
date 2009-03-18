
from test_utils import *
import sys

if not client_server(r'../examples/corba/echo/dynclient',
                     r'../examples/corba/echo/dynserver'):
    sys.exit(1)

