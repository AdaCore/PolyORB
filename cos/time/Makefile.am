include $(srcdir)/../Makefile.common

GNATMAKE_FLAGS = -z -I$(srcdir) \
	-I../event \
	-I$(srcdir)/../event \
	-I$(COS_DIRS) \
	-I../../src \
	-I$(srcdir)/../../src \
	$(ADASOCKETS_INCLUDE) \
	-g -O2 -m -gnata \
	-largs $(ADASOCKETS_LIBS)

PROGS = test_time

COS_DIRS=../../idls/cos/time

test_time: force
	$(GNATMAKE_FOR_TARGET) $@ $(GNATMAKE_FLAGS)
