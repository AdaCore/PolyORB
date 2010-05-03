
from test_utils import *
import sys

if not client_server(r'corba/interop/cpp/TAO/all_types_dynclient', r'',
                     r'../examples/corba/all_types/server', r'scenarios/polyorb_conf/broken_codesets.conf'):
    fail()

