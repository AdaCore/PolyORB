from test_utils import *
import os, shutil, sys

shutil.copy(os.path.join(SRC_DIR,
                         '..',
                         'examples', 'moma', 'destinations.conf'),
            '.')

if not client_server(r'../examples/moma/client_call_back', r'',
                     r'../examples/moma/server', r''):
    fail()

