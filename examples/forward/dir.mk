
FLAGS = $(ADABROKER_FLAGS) $(CORBA_LIB) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) ada
	gnatmake -gnatf -gnata -m -i client.adb $(FLAGS)
	gnatmake -gnatf -gnata -m -i server.adb $(FLAGS)


clean::
	-rm -f *.o *.ali *~ server client \
*-skeleton* *-marshal* *-proxies* chicken.ad* egg.ad* *_forward* \
chicken_idl_file.ads egg_idl_file.ads

ada: egg.ads chicken.ads

egg.ads: egg.idl
	omniidl2 -bada egg.idl

chicken.ads: chicken.idl
	omniidl2 -bada chicken.idl
