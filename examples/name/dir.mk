
FLAGS = $(ADABROKER_FLAGS) $(CORBA_LIB) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND)
	gnatmake -gnatf -gnata -m -i name_idl_file-marshal.adb $(FLAGS)
	gnatmake -gnatf -gnata -m -i test_utest-test-test-marshal.adb $(FLAGS)


clean::
	rm *.o *~ *.a*

ada:
	omniidl2 -b ada name.idl
