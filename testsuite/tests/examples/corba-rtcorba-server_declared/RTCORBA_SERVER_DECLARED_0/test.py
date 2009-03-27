
from test_utils import *
import sys

if not client_server(r'../examples/corba/echo/client', r'',
                     r'../examples/corba/rtcorba/server_declared/server', r''):
    sys.exit(1)

