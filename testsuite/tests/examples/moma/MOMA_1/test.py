
from test_utils import *
import sys

if not client_server(r'../examples/moma/client_call_back', r'',
                     r'../examples/moma/server', r''):
    fail()

