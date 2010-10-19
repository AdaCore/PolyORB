
from test_utils import *
import sys

if not client_server(r'corba/code_sets/test000/client', r'',
                     r'corba/code_sets/test000/server', r'code_sets_000_server.conf'):
    fail()

