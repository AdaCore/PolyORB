
from test_utils import *
import sys

if not client_server(r'../examples/corba/all_types/client', r'scenarios/polyorb_conf/soap.conf',
                     r'../examples/corba/all_types/server', r'scenarios/polyorb_conf/soap.conf'):
    sys.exit(1)

