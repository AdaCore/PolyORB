
from test_utils import *
import sys

if not client_server(r'corba/performance/client', r'scenarios/polyorb_conf/performance.conf',
                     r'corba/performance/server_no_tasking', r'scenarios/polyorb_conf/performance.conf'):
    sys.exit(1)

