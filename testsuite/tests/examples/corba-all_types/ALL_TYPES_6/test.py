
from test_utils import *
import sys

r1, r2 = (
    client_server(
        r'../examples/corba/all_types/client',
        r'scenarios/polyorb_conf/giop_1_1.conf',
        r'../examples/corba/all_types/server',
        r'scenarios/polyorb_conf/giop_1_1.conf'),
    local(
        r'../examples/corba/all_types/client',
        r'scenarios/polyorb_conf/giop_1_1.conf',
        args=['local']))
if not r1 or not r2:
    fail()

