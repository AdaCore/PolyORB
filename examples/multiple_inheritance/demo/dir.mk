
FLAGS = $(ADABROKER_FLAGS) $(CORBA_LIB) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND)
	gnatmake -gnatf -gnata -m -i client $(FLAGS)
	gnatmake -gnatf -gnata -m -i server $(FLAGS)


clean::
	rm *.o *.ali *~ server client vehicle.ad* tank.a* weapon.a* \
*-skeleton* *-marshal* *-proxies* chicken.ad* egg.ad* *_forward* \
classes.hh classesSK.cc

