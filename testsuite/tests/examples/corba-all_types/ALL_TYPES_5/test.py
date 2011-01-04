
from test_utils import *
import sys

r1, r2 = (
    client_server(
        r'../examples/corba/all_types/client',
        r'giop_1_0.conf',
        r'../examples/corba/all_types/server',
        r'giop_1_0.conf'),
    local(
        r'../examples/corba/all_types/client',
        r'giop_1_0.conf',
        args=['local']))
if not r1 or not r2:
    fail()

