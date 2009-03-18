
from test_utils import *
import sys

if not client_server(r'corba/benchs/test000/client',
                     r'corba/benchs/test000/server'):
    sys.exit(1)

