
from test_utils import *
import sys

r1, r2 = (
    client_server(
        r'../examples/corba/all_types/client',
        r'scenarios/polyorb_conf/ssliop.conf',
        r'../examples/corba/all_types/server',
        r'scenarios/polyorb_conf/ssliop.conf'),
    local(
        r'../examples/corba/all_types/client',
        r'scenarios/polyorb_conf/ssliop.conf',
        args=['local']))
if not r1 or not r2:
    fail()

