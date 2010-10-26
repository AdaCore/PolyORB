
from test_utils import *
import sys

if not client_server(r'../examples/corba/send/send', r'miop.conf',
                     r'../examples/corba/send/listener', r'miop.conf'):
    fail()

