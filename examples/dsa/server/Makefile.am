EXAMPLES_ROOT=../..
EXAMPLE = server

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir)/..

rci.ali: $(srcdir)/../rci.ads $(srcdir)/../rci.adb
	$(GNATMAKE) -c $(srcdir)/../rci.adb $(GNATMAKE_FLAGS) -cargs -gnatzr

server:: rci.ali

include $(srcdir)/../../Makefile.common
