all-local:: client server

client server: force all_types.ads
	$(GNATMAKE) $@ $(EXAMPLES_GNATMAKE_FLAGS)

all_types.ads: all_types.idl
	$(IDLAC) $(srcdir)/all_types.idl

DISTCLEANFILES = client server all_types.ads all_types.adb \
                        all_types-skel.ads all_types-skel.adb \
			all_types-stream.ads all_types-stream.adb
include $(srcdir)/../Makefile.common
