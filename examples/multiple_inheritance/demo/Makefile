ADABROKER_PATH = ../../../InterfaceORB

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

#include ../../../InterfaceORB/Makefile.sys_dep
OMNIORB_SYSDEP_FLAGS = -D __x86__ -D __linux__ -D __OSVERSION__=2
OMNIORB_LIBS = -L../../../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
-lomniORB2 -lomniDynamic2 -lomnithread -lpthread -ltcpwrapGK

main: .
	make -C ../../../InterfaceORB main
	gnatmake -gnatf -gnata -m -i client $(COMMAND_LINE) $(OMNIORB_LIBS)
	make server

server.o: server.cc classes.hh
	g++ -c -O2  -Wall -Wno-unused -D__OMNIORB2__ \
$(OMNIORB_SYSDEP_FLAGS) -D_REENTRANT -I.  -I../../../omniORB_2.7.0/include  -o server.o server.cc

classesSK.o: classes.hh classesSK.cc 
	g++ -c -O2  -Wall -Wno-unused -D__OMNIORB2__ \
$(OMNIORB_SYSDEP_FLAGS) -D_REENTRANT -I.  -I../../../omniORB_2.7.0/include  -o classesSK.o classesSK.cc

server: server.o classesSK.o
	g++ -o server -O2 -Wall -Wno-unused \
server.o classesSK.o $(OMNIORB_LIBS)

classes.hh: classes.idl
	omniidl2 classes.idl

classesSK.cc : classes.idl
	omniidl2 classes.idl

clean:
	rm *.ali *.o *~


