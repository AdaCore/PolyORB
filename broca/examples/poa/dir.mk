all:: client serverp

client:: $(CORBA_LIB_DEPEND) stamp-ada
	$(GNATMAKE) $(BROCA_FLAGS) -i client.adb

serverp:: $(CORBA_LIB_DEPEND) echo-my_impl.ad[sb] stamp-ada
	$(GNATMAKE) $(BROCA_FLAGS) -i serverp.adb


IDL_INTERFACE = echo

GENERATED_FILES = $(IDL_INTERFACE).ad*
GENERATED_FILES += $(IDL_INTERFACE)-proxy.ad*
GENERATED_FILES += $(IDL_INTERFACE)-stream.ad*
GENERATED_FILES += $(IDL_INTERFACE)-impl.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file-stream.ad*

clean::
	-rm -f b_*.c *.o *.ali *~ serverp client $(GENERATED_FILES)

stamp-ada:: $(IDL_INTERFACE).idl
	@echo Mapping IDL contract...
	$(ADABROKER) $(IDL_INTERFACE).idl
	touch stamp-ada
