include $(srcdir)/../Makefile.common

COS_GNATMAKE_FLAGS = -z -I$(srcdir) \
	-I../event \
	-I$(srcdir)/../event \
	-I$(COS_DIRS) \
	-I../../src \
	-I$(srcdir)/../../src

PROGS = test_time

COS_DIRS=../../idls/cos/time

test_time: force
	$(GNATMAKE_FOR_TARGET) $@ $(GNATMAKE_FLAGS)
