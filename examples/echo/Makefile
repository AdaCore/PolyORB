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

main: .
	make -C ../../InterfaceORB main
	gnatmake -gnatf -gnata -m -i client $(COMMAND_LINE)
	gnatmake -gnatf -gnata -m -i server $(COMMAND_LINE)

clean:
	rm *.ali *.o *~


