IDL_INTERFACE = echo

GENERATED_FILES = $(IDL_INTERFACE).ads
GENERATED_FILES += $(IDL_INTERFACE)-stream.ads
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ads
GENERATED_FILES += $(IDL_INTERFACE).adb

all:: client serverp

client:: $(CORBA_LIB_DEPEND) $(GENERATED_FILES)
	$(GNATMAKE) $(BROCA_FLAGS) -i client.adb

serverp:: $(CORBA_LIB_DEPEND) $(GENERATED_FILES)
	$(GNATMAKE) $(BROCA_FLAGS) -i serverp.adb

clean::
	-rm -f b_*.c *.o *.ali *~ serverp client $(GENERATED_FILES)

$(GENERATED_FILES):: $(IDL_INTERFACE).idl
	echo Mapping IDL contract...
	$(ADABROKER) $(IDL_INTERFACE).idl
