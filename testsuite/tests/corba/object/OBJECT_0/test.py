
from test_utils import *
import sys

if not client_server(r'corba/object/test000/test000_client', r'',
                     r'corba/object/test000/test000_server', r''):
    sys.exit(1)

