
FLAGS = $(ADABROKER_FLAGS) $(CORBA_LIB) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) echo.ads
	gnatmake -gnatf -gnata -m -i client.adb $(FLAGS)
	gnatmake -gnatf -gnata -m -i server.adb $(FLAGS)

IDL_INTERFACE = echo

GENERATED_FILES = $(IDL_INTERFACE).ad*
GENERATED_FILES += $(IDL_INTERFACE)-proxies.ad*
GENERATED_FILES += $(IDL_INTERFACE)-marshal.ad*
GENERATED_FILES += $(IDL_INTERFACE)-skeleton.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file-marshal.ad*

clean::
	rm *.o *.ali *~ server client $(GENERATED_FILES)

echo.ads: echo.idl
	omniidl2 -bada echo.idl
ada::
	omniidl2 -bada echo.idl


