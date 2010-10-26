
from test_utils import *
import sys

if not client_server(r'corba/portableinterceptor/test004/client', r'',
                     r'corba/portableinterceptor/test004/server', r''):
    fail()

