
FLAGS = $(ADABROKER_FLAGS) $(CORBA_LIB) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) all_exceptions.ads
	gnatmake -g -I.. -gnatf -gnata -i client.adb $(FLAGS)
	gnatmake -g -I.. -gnatf -gnata -i server.adb $(FLAGS)

IDL_INTERFACE = all_exceptions

GENERATED_FILES = $(IDL_INTERFACE).ad*
GENERATED_FILES += $(IDL_INTERFACE)-proxies.ad*
GENERATED_FILES += $(IDL_INTERFACE)-marshal.ad*
GENERATED_FILES += $(IDL_INTERFACE)-skeleton.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file-marshal.ad*

clean::
	-rm -f *.o *.ali *~ server client server $(GENERATED_FILES)

ada:
	omniidl2 -b ada all_exceptions.idl

all_exceptions.ads: all_exceptions.idl
	omniidl2 -bada all_exceptions.idl



