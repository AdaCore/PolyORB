FLAGS = -A$(EXPORT_TREE)/$(LIBDIR) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) ada
	gnatmake -g -I.. -gnatf -gnata -i client.adb $(FLAGS)
	gnatmake -g -I.. -gnatf -gnata -i server.adb $(FLAGS)

IDL_INTERFACE = all_exceptions

GENERATED_FILES = $(IDL_INTERFACE).ad*
GENERATED_FILES += $(IDL_INTERFACE)-proxy.ad*
GENERATED_FILES += $(IDL_INTERFACE)-stream.ad*
GENERATED_FILES += $(IDL_INTERFACE)-skel.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file-stream.ad*
GENERATED_FILES += b_*.c

clean::
	-rm -f *.o *.ali *~ server client server $(GENERATED_FILES)

ada:: all_exceptions.ads

all_exceptions.ads: all_exceptions.idl
	$(EXPORT_TREE)/$(BINDIR)/adabroker all_exceptions.idl

