FLAGS = -A$(EXPORT_TREE)/$(LIBDIR) $(IMPORT_LIBRARY_FLAGS)
 
all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND)
	gnatmake -g -gnatf -gnata -i dii_client.adb -I.. $(FLAGS)

IDL_INTERFACE = all_types

GENERATED_FILES =

clean::
	-rm -f *.o *.ali *~ dii_client  $(GENERATED_FILES)
