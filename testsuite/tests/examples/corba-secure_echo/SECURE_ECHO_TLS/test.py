from test_utils import *
import sys, os

conf_file = os.path.join(SRC_DIR,
                         '..',
                         'examples', 'corba', 'secure_echo', 'tls.conf')

if not client_server(r'../examples/corba/secure_echo/client', conf_file,
                     r'../examples/corba/secure_echo/server', conf_file):
    fail()

