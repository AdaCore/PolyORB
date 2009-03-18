
from test_utils import *
import sys

if not client_server(r'corba/cos/ir/client',
                     r'corba/cos/ir/server'):
    sys.exit(1)

