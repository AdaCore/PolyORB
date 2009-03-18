
from test_utils import *
import sys

if not client_server(r'../examples/corba/rtcorba/server_declared/client',
                     r'../examples/corba/rtcorba/server_declared/server'):
    sys.exit(1)

