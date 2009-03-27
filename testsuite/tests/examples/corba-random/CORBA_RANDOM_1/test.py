
from test_utils import *
import sys

if not client_server(r'../examples/corba/random/client', r'scenarios/polyorb_conf/soap.conf',
                     r'../examples/corba/random/server', r'scenarios/polyorb_conf/soap.conf'):
    sys.exit(1)

