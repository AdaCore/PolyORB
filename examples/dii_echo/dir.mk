FLAGS = -g -A../../interfaceorb $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND)
	gnatmake -gnatf -gnata -m -i dii_client.adb $(FLAGS)

IDL_INTERFACE = echo

GENERATED_FILES = 

clean::
	-rm -f *.o *.ali *~ dii_client b_*.c $(GENERATED_FILES)

