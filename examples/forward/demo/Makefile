
ADABROKER_PATH=../../..
include $(ADABROKER_PATH)/InterfaceORB/system_dependant.mk


COMMAND_LINE = -I$(INTERFACEORB_PATH) \
-largs $(CPP_LINK_FILES) $(OMNIORB_LIBS)

main:
	make -C ../../../InterfaceORB main
	gnatmake -gnatf -gnata -m -i client $(COMMAND_LINE)
	make server

clean:
	rm *.ali *.o *~ server client chicken_egg.hh chicken_eggSK.cc

server.o: server.cc chicken_egg.hh
	g++ -c -O2  -Wall -Wno-unused -D__OMNIORB2__ \
$(OMNIORB_SYSDEP_FLAGS) -D_REENTRANT -I.  -I$(OMNIORB_INCLUDES)  -o server.o server.cc

chicken_eggSK.o: chicken_egg.hh chicken_eggSK.cc 
	g++ -c -O2  -Wall -Wno-unused -D__OMNIORB2__ \
$(OMNIORB_SYSDEP_FLAGS) -D_REENTRANT -I.  -I$(OMNIORB_INCLUDES) -o chicken_eggSK.o chicken_eggSK.cc

server: server.o chicken_eggSK.o
	g++ -o server -O2 -Wall -Wno-unused \
server.o chicken_eggSK.o $(OMNIORB_LIBS)

chicken_egg.hh: chicken_egg.idl
	omniidl2 chicken_egg.idl

chicken_eggSK.cc : chicken_egg.idl
	omniidl2 chicken_egg.idl


