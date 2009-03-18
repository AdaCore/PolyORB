
from test_utils import *
import sys

if not client_server(r'../examples/corba/random/client',
                     r'../examples/corba/random/server'):
    sys.exit(1)

