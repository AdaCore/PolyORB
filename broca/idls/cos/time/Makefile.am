include $(srcdir)/../../Makefile.common

CosTime_IDL=CosTime.idl
CosTime_GEN= \
  costime-helper.adb \
  costime-helper.ads \
  costime-stream.adb \
  costime-stream.ads \
  costime-timeservice-helper.adb \
  costime-timeservice-helper.ads \
  costime-timeservice-skel.adb \
  costime-timeservice-skel.ads \
  costime-timeservice-stream.adb \
  costime-timeservice-stream.ads \
  costime-timeservice.adb \
  costime-timeservice.ads \
  costime-tio-helper.adb \
  costime-tio-helper.ads \
  costime-tio-skel.adb \
  costime-tio-skel.ads \
  costime-tio-stream.adb \
  costime-tio-stream.ads \
  costime-tio.adb \
  costime-tio.ads \
  costime-uto-helper.adb \
  costime-uto-helper.ads \
  costime-uto-skel.adb \
  costime-uto-skel.ads \
  costime-uto-stream.adb \
  costime-uto-stream.ads \
  costime-uto.adb \
  costime-uto.ads \
  costime.adb \
  costime.ads \
  costime_idl_file.ads \
  timebase-helper.adb \
  timebase-helper.ads \
  timebase-stream.adb \
  timebase-stream.ads \
  timebase.ads \
  timebase_idl_file.ads

DEPS=$(CosTime_GEN)

$(CosTime_GEN): $(CosTime_IDL)
	$(IDLAC) $< -I$(srcdir)
