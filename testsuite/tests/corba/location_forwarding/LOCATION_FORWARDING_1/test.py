
from test_utils import *
import sys

if not client_server(r'corba/location_forwarding/test001/test001_client',
                     r'corba/location_forwarding/test001/test001_server'):
    sys.exit(1)

