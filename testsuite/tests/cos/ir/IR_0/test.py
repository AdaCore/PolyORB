
from test_utils import *
import sys

if not client_server(r'corba/cos/ir/client', r'',
                     r'corba/cos/ir/server', r''):
    fail()

