
from test_utils import *
import sys

if not client_server(r'corba/all_exceptions/client',
                     r'corba/all_exceptions/server'):
    sys.exit(1)

