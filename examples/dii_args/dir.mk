FLAGS = -g -A$(EXPORT_TREE)/$(LIBDIR) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) ada
	gnatmake -gnatf -gnata -m -i server.adb $(FLAGS)
	gnatmake -gnatf -gnata -m -i client.adb $(FLAGS)
	gnatmake -gnatf -gnata -m -i dii_client.adb $(FLAGS)

IDL_INTERFACE = args

GENERATED_FILES = $(IDL_INTERFACE).ad*
GENERATED_FILES += $(IDL_INTERFACE)-proxy.ad*
GENERATED_FILES += $(IDL_INTERFACE)-stream.ad*
GENERATED_FILES += $(IDL_INTERFACE)-skel.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file-stream.ad*

clean::
	-rm -f *.o *.ali *~ server client dii_client b_*.c $(GENERATED_FILES)

clean_dii_client:
	rm dii_client dii_client.ali dii_client.o b_dii_client*

args.ads: args.idl
	$(EXPORT_TREE)/$(BINDIR)/adabroker -i args.idl

ada:: args.ads
