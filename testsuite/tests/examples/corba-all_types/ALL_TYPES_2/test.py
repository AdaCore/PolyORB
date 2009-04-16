
from test_utils import *
import sys

if not client_server(r'../examples/corba/all_types/dynclient', r'',
                     r'../examples/corba/all_types/server', r''):
    sys.exit(1)

