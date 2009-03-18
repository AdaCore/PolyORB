
from test_utils import *
import sys

if not client_server(r'../examples/corba/all_functions/dynclient',
                     r'../examples/corba/all_functions/server'):
    sys.exit(1)

