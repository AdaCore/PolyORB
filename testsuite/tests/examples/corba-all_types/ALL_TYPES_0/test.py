
from test_utils import *
import sys

r1, r2 = (
    client_server(r'../examples/corba/all_types/client', r'',
                  r'../examples/corba/all_types/server', r''),
    local(r'../examples/corba/all_types/client', r'', args=['local']))

if not r1 or not r2:
    fail()

