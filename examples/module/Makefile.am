all-local:: client server

client server: force module.ads
	$(GNATMAKE) $@ $(EXAMPLES_GNATMAKE_FLAGS)

module.ads: module.idl
	$(IDLAC) $(srcdir)/module.idl

DISTCLEANFILES = client server module.ads module.adb \
                        module-skel.ads module-skel.adb \
			module-stream.ads module-stream.adb
include $(srcdir)/../Makefile.common
