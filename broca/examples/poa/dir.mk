all:: client serverp

client:: $(CORBA_LIB_DEPEND) ada
	$(GNATMAKE) $(GNATMAKE_FLAGS) -i client.adb

server:: $(CORBA_LIB_DEPEND) ada
	$(GNATMAKE) $(GNATMAKE_FLAGS) -i server.adb


IDL_INTERFACE = echo

GENERATED_FILES = $(IDL_INTERFACE).ad*
GENERATED_FILES += $(IDL_INTERFACE)-proxy.ad*
GENERATED_FILES += $(IDL_INTERFACE)-stream.ad*
GENERATED_FILES += $(IDL_INTERFACE)-skel.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file-stream.ad*

clean::
	-rm -f b_*.c *.o *.ali *~ serverp client $(GENERATED_FILES)

ada::
	$(ADABROKER) $(IDL_INTERFACE).idl
