BROCA_FLAGS += -I../generic

# for hand-made examples that are in the hand-made directory
BROCA_FLAGS += -I../../generic


all:: client server

#client:: $(CORBA_LIB_DEPEND) $(GENERATED_FILES) client.adb
client:: $(GENERATED_FILES) force
	$(GNATMAKE) $(BROCA_FLAGS) -i client.adb

#server:: $(CORBA_LIB_DEPEND) $(GENERATED_FILES) server.adb ../generic/genericserver.ads ../generic/genericserver.adb
server:: $(GENERATED_FILES) force
	$(GNATMAKE) $(BROCA_FLAGS) -i server.adb

$(GENERATED_FILES):: $(IDL_INTERFACE).idl
	@echo Mapping IDL contract...
	$(ADABROKER) $(IDL_INTERFACE).idl

stubs:: $(IDL_INTERFACE).idl
	$(ADABROKER) -i $(IDL_INTERFACE).idl

force::

clean::
	-rm -f b~*.ad[sb] *.o *.ali *~ server client $(GENERATED_FILES)

