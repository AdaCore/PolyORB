all-local:: client server

client server: force random.ads
	$(GNATMAKE) $@ $(EXAMPLES_GNATMAKE_FLAGS)

random.ads: random.idl
	$(IDLAC) $(srcdir)/random.idl

DISTCLEANFILES = client server random.ads random.adb \
                        random-skel.ads random-skel.adb \
			random-stream.ads random-stream.adb
include $(srcdir)/../Makefile.common
