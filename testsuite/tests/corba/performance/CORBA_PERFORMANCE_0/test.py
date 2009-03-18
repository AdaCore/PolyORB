
from test_utils import *
import sys

if not client_server(r'corba/performance/client',
                     r'corba/performance/server_no_tasking'):
    sys.exit(1)

