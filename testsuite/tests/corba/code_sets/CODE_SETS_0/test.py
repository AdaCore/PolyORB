
from test_utils import *
import sys

if not client_server(r'corba/code_sets/test000/client',
                     r'corba/code_sets/test000/server'):
    sys.exit(1)

