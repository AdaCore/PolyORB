
FLAGS = $(ADABROKER_FLAGS) $(CORBA_LIB) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) mpeg.ads
	gnatmake -gnatf -gnata -m -i client.adb $(FLAGS)

clean::
	rm *.o *.ali *~ *marshal* *proxies* *skeleton* mpeg* \
dominante_idl_file.ads

ada: dominante.idl
	omniidl2 -bada dominante.idl

mpeg.ads: dominante.idl
	omniidl2 -bada dominante.idl
