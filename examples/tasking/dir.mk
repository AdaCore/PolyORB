FLAGS = -g -A$(EXPORT_TREE)/$(LIBDIR) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) ada
	gnatmake -I.. -gnatf -gnata -m -i client.adb $(FLAGS)
	gnatmake -I.. -gnatf -gnata -m -i server.adb $(FLAGS)

IDL_INTERFACE = sema

GENERATED_FILES = $(IDL_INTERFACE).ad*
GENERATED_FILES += $(IDL_INTERFACE)-proxy.ad*
GENERATED_FILES += $(IDL_INTERFACE)-stream.ad*
GENERATED_FILES += $(IDL_INTERFACE)-skel.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file-stream.ad*

clean::
	-rm -f *.o *.ali *~ server client b_*.c $(GENERATED_FILES)

sema.ads: sema.idl
	$(EXPORT_TREE)/$(BINDIR)/adabroker -i sema.idl

ada:: sema.ads
