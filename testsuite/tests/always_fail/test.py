from test_utils import *
import sys

local(os.path.join(os.path.pardir, 'polyorb-config'), '', args=['--version'])
fail()

