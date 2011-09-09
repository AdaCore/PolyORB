
from test_utils import *
import sys

if not client_server(r'corba/interop/cpp/omniORB/all_types_client', r'',
                     r'../examples/corba/all_types/server', r''):
    fail()

