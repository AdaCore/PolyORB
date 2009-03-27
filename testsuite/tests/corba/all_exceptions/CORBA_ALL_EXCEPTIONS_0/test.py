
from test_utils import *
import sys

if not client_server(r'corba/all_exceptions/client', r'',
                     r'corba/all_exceptions/server', r''):
    sys.exit(1)

