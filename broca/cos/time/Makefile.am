include $(srcdir)/../Makefile.common

GNATMAKE_FLAGS = -I$(srcdir) \
	-I../event \
	-I$(srcdir)/../event \
	-I../../src \
	-I$(srcdir)/../../src \
	-I../../adasockets/src \
	-I$(srcdir)/../../adasockets/src \
	-g -O2 -m -gnata

PROGRAM = test_time

CLEANFILES += $(GEN_COSTIME)

GEN_COSTIME =	costime.ads costime.adb \
		costime-helper.ads costime-helper.adb \
		costime-stream.ads costime-stream.adb \
		costime_idl_file.ads \
		costime-tio.ads costime-tio.adb \
		costime-tio-helper.ads costime-tio-helper.adb \
		costime-tio-stream.ads costime-tio-stream.adb \
		costime-tio-skel.ads costime-tio-skel.adb \
		costime-uto.ads costime-uto.adb \
		costime-uto-helper.ads costime-uto-helper.adb \
		costime-uto-stream.ads costime-uto-stream.adb \
		costime-uto-skel.ads costime-uto-skel.adb \
		costime-timeservice.ads costime-timeservice.adb \
		costime-timeservice-helper.ads costime-timeservice-helper.adb \
		costime-timeservice-stream.ads \
		costime-timeservice-stream.adb \
		costime-timeservice-skel.ads costime-timeservice-skel.adb \
		timebase.ads \
		timebase_idl_file.ads \
		timebase-helper.ads timebase-helper.adb \
		timebase-stream.ads timebase-stream.adb

$(GEN_COSTIME): CosTime.idl TimeBase.idl $(IDLAC)
	$(IDLAC) -I$(srcdir) $(srcdir)/CosTime.idl

test_time: force $(GEN_COSTIME)
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)
