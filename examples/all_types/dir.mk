FLAGS = -A../../InterfaceORB $(IMPORT_LIBRARY_FLAGS)
 
all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) all_types.ads
	gnatmake -g -gnatf -gnata -i client.adb -I.. $(FLAGS)
	gnatmake -g -gnatf -gnata -i server.adb -I.. $(FLAGS)

IDL_INTERFACE = all_types

GENERATED_FILES = $(IDL_INTERFACE).ad*
GENERATED_FILES += $(IDL_INTERFACE)-proxy.ad*
GENERATED_FILES += $(IDL_INTERFACE)-stream.ad*
GENERATED_FILES += $(IDL_INTERFACE)-skel.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file-stream.ad*

clean::
	-rm -f *.o *.ali *~ server client server $(GENERATED_FILES)

ada:
	omniidl2 -b ada all_types.idl

all_types.ads: all_types.idl
	omniidl2 -b ada all_types.idl



