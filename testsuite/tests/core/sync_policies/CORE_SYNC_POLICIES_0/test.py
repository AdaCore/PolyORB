
from test_utils import *
import sys

if not client_server(r'core/sync_policies/client', r'',
                     r'core/sync_policies/server_no_tasking', r''):
    sys.exit(1)

