IDL_INTERFACE = echo

GENERATED_FILES =\
echo-skel.adb\
echo-skel.ads\
echo-stream.adb\
echo-stream.ads\
echo.adb\
echo.ads\
echo_idl_file.ads


BROCA_FLAGS += -I../generic

all:: client server

#client:: $(CORBA_LIB_DEPEND) $(GENERATED_FILES) client.adb
client:: $(GENERATED_FILES) force
	$(GNATMAKE) $(BROCA_FLAGS) -i client.adb

#server:: $(CORBA_LIB_DEPEND) $(GENERATED_FILES) server.adb ../generic/genericserver.ads ../generic/genericserver.adb
server:: $(GENERATED_FILES) force
	$(GNATMAKE) $(BROCA_FLAGS) -i server.adb

$(GENERATED_FILES):: $(IDL_INTERFACE).idl
	@echo Not mapping IDL contract...

stubs:: $(IDL_INTERFACE).idl
	$(ADABROKER) -i $(IDL_INTERFACE).idl

force::

clean::
	-rm -f b~*.ad[sb] *.o *.ali *~ server client $(GENERATED_FILES)

