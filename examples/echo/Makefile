GNATGCC = gnatgcc -gnatf
DO_NOT_LINK = -c
BIND = gnatbind
LINK = gnatlink

ADABROKER_PATH = ../../InterfaceORB
ORBCORE_PATH = ../../omniORB_2.7.0/src/lib/omniORB2/orbcore

CPP_LINK_FILES = $(ADABROKER_PATH)/Ada_Giop_c.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_Giop_s.o
CPP_LINK_FILES += $(ADABROKER_PATH)/omniObject_C2Ada.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_OmniObject.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_OmniRopeAndKey.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_Rope.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_netBufferedStream.o
CPP_LINK_FILES += $(ADABROKER_PATH)/Ada_memBufferedStream.o
CPP_LINK_FILES += $(ADABROKER_PATH)/proxyObjectFactory_C2Ada.o



main:
	make -C ../../InterfaceORB main
	make client
#	make server

client: echo.o echo-proxies.o client.o
	$(BIND) -I$(ADABROKER_PATH) client.ali
	$(LINK)  \
-Wl,-rpath,../../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
-L../../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
client.ali $(CPP_LINK_FILES) \
-lomniORB2 -lomniDynamic2 -lomnithread -lpthread -ltcpwrapGK

server: echo.o echo-skeleton.o server.o
	$(BIND) -I$(ADABROKER_PATH) server.ali
	$(LINK)  \
-Wl,-rpath,../../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
-L../../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
server.ali $(CPP_LINK_FILES) \
-lomniORB2 -lomniDynamic2 -lomnithread -lpthread -ltcpwrapGK



.SUFFIXES: .adb .o

.adb.o:
	$(GNATGCC)  $(DO_NOT_LINK) -I$(ADABROKER_PATH) $<

echo.o : echo.ads echo.adb echo-proxies.ads echo-proxies.adb $(ADABROKER_PATH)

echo-impl.o : echo-impl.ads echo-impl.adb echo.o $(ADABROKER_PATH) 

echo-proxies.o: echo-proxies.ads echo-proxies.adb echo.o $(ADABROKER_PATH)

echo-skeleton.o: echo-skeleton.ads echo-skeleton.adb echo.o $(ADABROKER_PATH)

server.o: echo.ads echo-impl.ads $(ADABROKER_PATH)

client.o: echo.ads $(ADABROKER_PATH)

clean:
	rm *.ali *.o


