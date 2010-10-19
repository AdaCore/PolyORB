
from test_utils import *
import sys

if not client_server(r'corba/performance/client', r'performance.conf',
                     r'corba/performance/server_no_tasking', r'performance.conf'):
    fail()

