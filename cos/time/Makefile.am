include $(srcdir)/../Makefile.common

GNATMAKE_FLAGS = -z -I$(srcdir) \
	-I../event \
	-I$(srcdir)/../event \
	-I$(COS_DIRS) \
	-I../../src \
	-I$(srcdir)/../../src \
	-I../../adasockets/src \
	-I$(srcdir)/../../adasockets/src \
	-g -O2 -m -gnata

PROGS = test_time

COS_DIRS=../../idls/cos/time

test_time: force
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)
