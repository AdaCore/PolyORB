EXAMPLES_ROOT=../..
EXAMPLE = client

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir)/..

rci.ali: $(srcdir)/../rci.ads
	$(GNATMAKE) -c $(srcdir)/../rci.ads $(GNATMAKE_FLAGS) -cargs -gnatzc -gnatD

client:: rci.ali

include $(srcdir)/../../Makefile.common
