
from test_utils import *
import sys

if not client_server(r'corba/interop/cpp/omniORB/all_functions_client',
                     r'../examples/corba/all_functions/server'):
    sys.exit(1)

