
from test_utils import *
import sys

if not client_server(r'../examples/corba/rtcorba/client_propagated/client',
                     r'../examples/corba/rtcorba/client_propagated/server'):
    sys.exit(1)

