
from test_utils import *
import sys

if not client_server(r'corba/domainmanager/test000/client',
                     r'corba/domainmanager/test000/server'):
    sys.exit(1)

