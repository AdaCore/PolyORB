all-local:: client server

client server: force all_functions.ads
	$(GNATMAKE) $@ $(EXAMPLES_GNATMAKE_FLAGS)

all_functions.ads: all_functions.idl
	$(IDLAC) $(srcdir)/all_functions.idl

DISTCLEANFILES = client server all_functions.ads all_functions.adb \
                        all_functions-skel.ads all_functions-skel.adb \
			all_functions-stream.ads all_functions-stream.adb
include $(srcdir)/../Makefile.common
