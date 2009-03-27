
from test_utils import *
import sys

if not client_server(r'../examples/corba/rtcorba/client_propagated/client', r'',
                     r'../examples/corba/rtcorba/client_propagated/server', r''):
    sys.exit(1)

