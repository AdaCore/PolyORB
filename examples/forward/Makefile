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

COMMAND_LINE = -I$(ADABROKER_PATH) \
-largs $(CPP_LINK_FILES) \
-L../../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
-lomniORB2 -lomniDynamic2 -lomnithread -lpthread -ltcpwrapGK

main:
	make -C ../../InterfaceORB main
	gnatmake -gnatf -gnata -m -i client $(COMMAND_LINE)
	make server

clean:
	rm *.ali *.o *~

server.o: server.cc
	/usr/local/egcs/bin/g++ -c -O2  -Wall -Wno-unused -D__OMNIORB2__ \
-D_REENTRANT -I.  -I../../omniORB_2.7.0/include -D__x86__ -D__linux__ \
-D__OSVERSION__=2 -o server.o server.cc

chickenSK.o: chicken.hh chickenSK.cc egg.hh
	/usr/local/egcs/bin/g++ -c -O2  -Wall -Wno-unused -D__OMNIORB2__ \
-D_REENTRANT -I.  -I../../omniORB_2.7.0/include -D__x86__ -D__linux__ \
-D__OSVERSION__=2 -o chickenSK.o chickenSK.cc

eggSK.o: egg.hh eggSK.cc chicken.hh
	/usr/local/egcs/bin/g++ -c -O2  -Wall -Wno-unused -D__OMNIORB2__ \
-D_REENTRANT -I.  -I../../omniORB_2.7.0/include -D__x86__ -D__linux__ \
-D__OSVERSION__=2 -o eggSK.o eggSK.cc

server: server.o chickenSK.o eggSK.o
	/usr/local/egcs/bin/g++ -o server -O2 -Wall -Wno-unused \
-Wl,-rpath,../../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
-L../../omniORB_2.7.0/lib/i586_linux_2.0_glibc server.o chickenSK.o \
eggSK.o -lomniORB2 -lomniDynamic2 -lomnithread -lpthread -ltcpwrapGK