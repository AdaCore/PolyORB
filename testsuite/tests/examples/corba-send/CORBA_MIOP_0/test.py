
from test_utils import *
import sys

if not client_server(r'../examples/corba/send/send', r'scenarios/polyorb_conf/miop.conf',
                     r'../examples/corba/send/listener', r'scenarios/polyorb_conf/miop.conf'):
    fail()

