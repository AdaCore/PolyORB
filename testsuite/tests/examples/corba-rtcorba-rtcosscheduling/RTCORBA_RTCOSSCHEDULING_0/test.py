
from test_utils import *
import sys

if not client_server(r'../examples/corba/rtcorba/rtcosscheduling/client', r'',
                     r'../examples/corba/rtcorba/rtcosscheduling/server', r''):
    fail()

