main:
	gnatmake -O2 -g -i -m -gnata -gnatf -gnatwu -gnatwl -I../../../adasockets/lib/adasockets -I../../../src -I.. -I../generic -I../../generic -i standalone.adb

IDL_INTERFACE = myexceptions

GENERATED_FILES =\
myexcpetions-skel.adb\
myexcpetions-skel.ads\
myexcpetions-stream.adb\
myexcpetions-stream.ads\
myexcpetions.adb\
myexcpetions.ads\
myexcpetions_idl_file.ads

include ../../generic/dir.mk

