
FLAGS = $(ADABROKER_FLAGS) $(CORBA_LIB) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND)
	gnatmake -gnatf -gnata -i client.adb $(FLAGS)
	gnatmake -gnatf -gnata -i server.adb $(FLAGS)

clean::
	rm *.o *.ali *~ server client server $(GENERATED_FILES)

ada:
	omniidl2 -b ada classes.idl





