all-local:: client server

client server: force echo.ads
	$(GNATMAKE) $@ $(EXAMPLES_GNATMAKE_FLAGS)

echo.ads: echo.idl
	$(IDLAC) $(srcdir)/echo.idl

DISTCLEANFILES = client server echo.ads echo.adb echo-skel.ads echo-skel.adb \
			echo-stream.ads echo-stream.adb
include $(srcdir)/../Makefile.common
