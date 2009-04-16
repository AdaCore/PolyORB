
from test_utils import *
import sys

if not client_server(r'../examples/corba/random/client', r'',
                     r'../examples/corba/random/server', r''):
    sys.exit(1)

