
from test_utils import *
import sys

if not client_server(r'../examples/moma/client',
                     r'../examples/moma/server'):
    sys.exit(1)

