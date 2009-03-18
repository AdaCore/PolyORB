
from test_utils import *
import sys

if not client_server(r'corba/portableinterceptor/test004/client',
                     r'corba/portableinterceptor/test004/server'):
    sys.exit(1)

