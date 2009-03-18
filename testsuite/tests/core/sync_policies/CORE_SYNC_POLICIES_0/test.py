
from test_utils import *
import sys

if not client_server(r'core/sync_policies/client',
                     r'core/sync_policies/server_no_tasking'):
    sys.exit(1)

