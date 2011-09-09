
from test_utils import *
import sys

if not client_server(r'corba/cos/naming/test_naming_corba', r'',
                     r'../tools/po_cos_naming/po_cos_naming', r''):
    fail()

