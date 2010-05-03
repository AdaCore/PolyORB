
from test_utils import *
import sys

if not client_server(r'corba/harness/client', r'',
                     r'corba/harness/server_no_tasking2', r''):
    fail()

