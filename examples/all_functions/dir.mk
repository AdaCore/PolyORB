FLAGS = -A../../InterfaceORB $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) all_functions.ads
	gnatmake -I.. -gnatf -gnata -m -i client.adb $(FLAGS)
	gnatmake -I.. -gnatf -gnata -m -i server.adb $(FLAGS)

IDL_INTERFACE = all_functions

GENERATED_FILES = $(IDL_INTERFACE).ad*
GENERATED_FILES += $(IDL_INTERFACE)-proxy.ad*
GENERATED_FILES += $(IDL_INTERFACE)-stream.ad*
GENERATED_FILES += $(IDL_INTERFACE)-skel.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file-stream.ad*

clean::
	-rm -f *.o *.ali *~ server client $(GENERATED_FILES)

ada::
	omniidl2 -b ada all_functions.idl

all_functions.ads: all_functions.idl
	omniidl2 -bada all_functions.idl

