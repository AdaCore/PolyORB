from test_utils import *
import os, shutil, sys

shutil.copyfile(os.path.join(SRC_DIR,
                             '..',
                             'examples', 'moma', 'destinations.conf'),
                '.')

if not client_server(r'../examples/moma/client', r'',
                     r'../examples/moma/server', r''):
    fail()

