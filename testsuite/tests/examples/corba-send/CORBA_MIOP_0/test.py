
from test_utils import *
import sys

if not client_server(r'../examples/corba/send/send',
                     r'../examples/corba/send/listener'):
    sys.exit(1)

