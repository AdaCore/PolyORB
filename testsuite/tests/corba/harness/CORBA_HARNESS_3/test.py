
from test_utils import *
import sys

if not client_server(r'corba/harness/client',
                     r'corba/harness/server_thread_per_session'):
    sys.exit(1)

