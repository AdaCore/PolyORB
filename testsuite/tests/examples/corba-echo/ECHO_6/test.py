
from test_utils import *
import sys

if not client_server(r'../examples/corba/echo/client', r'scenarios/polyorb_conf/giop_1_1.conf',
                     r'../examples/corba/echo/server', r'scenarios/polyorb_conf/giop_1_1.conf'):
    fail()

