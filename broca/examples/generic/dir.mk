BROCA_FLAGS += -I../generic

all:: client server

client:: $(CORBA_LIB_DEPEND) $(GENERATED_FILES) client.adb
	$(GNATMAKE) $(BROCA_FLAGS) -i client.adb

server:: $(CORBA_LIB_DEPEND) $(GENERATED_FILES) server.adb ../generic/genericserver.ads ../generic/genericserver.adb
	$(GNATMAKE) $(BROCA_FLAGS) -i server.adb

$(GENERATED_FILES):: $(IDL_INTERFACE).idl
	@echo Mapping IDL contract...
	$(ADABROKER) $(IDL_INTERFACE).idl

stubs:: $(IDL_INTERFACE).idl
	$(ADABROKER) -i $(IDL_INTERFACE).idl

clean::
	-rm -f b_*.c *.o *.ali *~ server client $(GENERATED_FILES)

