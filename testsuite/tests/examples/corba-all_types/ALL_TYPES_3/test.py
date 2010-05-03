
from test_utils import *
import sys

r1, r2 = (
    client_server(
        r'../examples/corba/all_types/dynclient',
        r'scenarios/polyorb_conf/soap.conf',
        r'../examples/corba/all_types/server',
        r'scenarios/polyorb_conf/soap.conf'),
    local(
        r'../examples/corba/all_types/dynclient',
        r'scenarios/polyorb_conf/soap.conf',
        args=['local']))
if not r1 or not r2:
    fail()

