
FLAGS = $(ADABROKER_FLAGS) $(CORBA_LIB) $(IMPORT_LIBRARY_FLAGS)

all:: $(CORBA_LIB_DEPEND) $(ADABROKER_LIB_DEPEND) weapon.ads
	gnatmake -I.. -gnatf -gnata -i client.adb -g $(FLAGS) 
	gnatmake -I.. -gnatf -gnata -i server.adb -g $(FLAGS)

GENERATED_FILES = *-proxies.ad*
GENERATED_FILES += *-marshal.ad*
GENERATED_FILES += *-skeleton.ad*
GENERATED_FILES += *_idl_file.ad*
GENERATED_FILES += *_idl_file-marshal.ad*
GENERATED_FILES += weapon.ad*
GENERATED_FILES += tank.ad*
GENERATED_FILES += vehicle.ad*


clean::
	-rm -f *.o *.ali *~ client server $(GENERATED_FILES)

ada:
	omniidl2 -b ada classes.idl

weapon.ads: classes.idl
	omniidl2 -bada classes.idl



