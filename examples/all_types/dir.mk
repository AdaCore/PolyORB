FLAGS = -A$(EXPORT_TREE)/$(LIBDIR) $(IMPORT_LIBRARY_FLAGS)
 
all:: client server

client:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) ada
	gnatmake -g -gnatf -gnata -i client.adb -I.. $(FLAGS)

server:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) ada
	gnatmake -g -gnatf -gnata -i server.adb -I.. $(FLAGS)


IDL_INTERFACE = all_types

GENERATED_FILES = $(IDL_INTERFACE).ad*
GENERATED_FILES += $(IDL_INTERFACE)-proxy.ad*
GENERATED_FILES += $(IDL_INTERFACE)-stream.ad*
GENERATED_FILES += $(IDL_INTERFACE)-skel.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file.ad*
GENERATED_FILES += $(IDL_INTERFACE)_idl_file-stream.ad*

clean::
	-rm -f *.o *.ali *~ server client $(GENERATED_FILES)

ada:: all_types.ads

all_types.ads: all_types.idl
	$(EXPORT_TREE)/$(BINDIR)/adabroker all_types.idl

