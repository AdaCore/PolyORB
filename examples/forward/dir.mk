FLAGS = -A$(EXPORT_TREE)/$(LIBDIR) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) ada
	gnatmake -g -I.. -gnatf -gnata -m -i client.adb $(FLAGS)
	gnatmake -g -I.. -gnatf -gnata -m -i server.adb $(FLAGS)

clean::
	-rm -f *.o *.ali *~ server client b_*.c\
	*-skel* *-stream* *-proxy* chicken.ad* egg.ad* *_forward* \
	chicken_idl_file.ads egg_idl_file.ads

ada: egg.ads chicken.ads

egg.ads: egg.idl
	$(EXPORT_TREE)/$(BINDIR)/adabroker -i egg.idl

chicken.ads: chicken.idl
	$(EXPORT_TREE)/$(BINDIR)/adabroker -i chicken.idl
