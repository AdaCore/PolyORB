ADABROKER_PATH = ../../InterfaceORB

CPP_LINK_FILES = $(ADABROKER_PATH)/Ada_Giop_c.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_Giop_s.o
CPP_LINK_FILES += $(ADABROKER_PATH)/omniObject_C2Ada.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_OmniObject.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_OmniRopeAndKey.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_netBufferedStream.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_memBufferedStream.o
CPP_LINK_FILES += $(ADABROKER_PATH)/proxyObjectFactory_C2Ada.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_Corba_Orb.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_Corba_Exceptions.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_exceptions.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_Iop.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_Corba_Boa.o

include ../../InterfaceORB/system_dependant.mk

COMMAND_LINE = -I$(ADABROKER_PATH) \
-largs $(CPP_LINK_FILES) $(OMNIORB_LIBS)

main:
	make -C ../../InterfaceORB main
	gnatmake -gnatf -gnata -m -i client $(COMMAND_LINE)
	make server

clean:
	rm *.ali *.o *~ server client chicken_egg.hh chicken_eggSK.cc

server.o: server.cc chicken_egg.hh
	g++ -c -O2  -Wall -Wno-unused -D__OMNIORB2__ \
$(OMNIORB_SYSDEP_FLAGS) -D_REENTRANT -I.  -I../../omniORB_2.7.0/include  -o server.o server.cc

chicken_eggSK.o: chicken_egg.hh chicken_eggSK.cc 
	g++ -c -O2  -Wall -Wno-unused -D__OMNIORB2__ \
$(OMNIORB_SYSDEP_FLAGS) -D_REENTRANT -I.  -I../../omniORB_2.7.0/include  -o chicken_eggSK.o chicken_eggSK.cc

server: server.o chicken_eggSK.o
	g++ -o server -O2 -Wall -Wno-unused \
server.o chicken_eggSK.o $(OMNIORB_LIBS)

chicken_egg.hh: chicken_egg.idl
	omniidl2 chicken_egg.idl

chicken_eggSK.cc : chicken_egg.idl
	omniidl2 chicken_egg.idl

