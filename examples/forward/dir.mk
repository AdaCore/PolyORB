
FLAGS = $(ADABROKER_FLAGS) $(CORBA_LIB) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) server chicken.ads egg.ads
	gnatmake -gnatf -gnata -m -i client $(FLAGS)


clean::
	rm *.o *.ali *~ server client chicken_egg.hh chicken_eggSK.cc


server: chicken_egg.o server.o $(CORBA_LIB_DEPEND)
	@(libs="$(CORBA_LIB)"; $(CXXExecutable))

chicken_eggSK.cc : chicken_egg.idl
	omniidl2 chicken_egg.idl

chicken_egg.o: chicken_eggSK.cc chicken_egg.hh 
	$(CXX) -c $(CXXFLAGS) -o $@ $<

egg.ads: egg.idl
	omniidl2 -bada egg.idl

chicken.ads: chicken.idl
	omniidl2 -bada chicken.idl
