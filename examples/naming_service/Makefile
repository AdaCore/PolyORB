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

COMMAND_LINE = -I$(ADABROKER_PATH) -I../echo\
-largs $(CPP_LINK_FILES) \

include ../../InterfaceORB/Makefile.sys_dep

main: .
	make -C ../echo main
	make -C $(ADABROKER_PATH) main
	gnatmake -gnata -m -i client $(COMMAND_LINE) $(OMNIORB_LIBS)

genere:
	../../omniidl2/omniidl2 -b ada Naming.idl
	../../omniidl2/omniidl2 -b ada bootstrap.idl

clean:
	rm *.ali *.o *~







