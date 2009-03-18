
from test_utils import *
import sys

if not client_server(r'../examples/corba/all_types/client',
                     r'../examples/corba/all_types/server'):
    sys.exit(1)

