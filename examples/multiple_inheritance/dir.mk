FLAGS = -A$(EXPORT_TREE)/$(LIBDIR) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) ada
	gnatmake -I.. -gnatf -gnata -i client.adb -g $(FLAGS) 
	gnatmake -I.. -gnatf -gnata -i server.adb -g $(FLAGS)

GENERATED_FILES = *-proxy.ad*
GENERATED_FILES += *-stream.ad*
GENERATED_FILES += *-skel.ad*
GENERATED_FILES += *_idl_file.ad*
GENERATED_FILES += *_idl_file-stream.ad*
GENERATED_FILES += weapon.ad*
GENERATED_FILES += tank.ad*
GENERATED_FILES += vehicle.ad*
GENERATED_FILES += b_*.c

clean::
	-rm -f *.o *.ali *~ client server $(GENERATED_FILES)

ada:: weapon.ads tank.ads vehicle.ads

weapon.ads tank.ads vehicle.ads: classes.idl
	$(EXPORT_TREE)/$(BINDIR)/adabroker classes.idl

