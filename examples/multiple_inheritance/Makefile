GNATGCC = gnatgcc
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

client: vehicle.o weapon.o tank.o client.o 
	$(BIND) -I$(ADABROKER_PATH) client.ali
	$(LINK)  \
-Wl,-rpath,../../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
-L../../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
client.ali $(CPP_LINK_FILES) \
-lomniORB2 -lomniDynamic2 -lomnithread -lpthread -ltcpwrapGK


.SUFFIXES: .adb .o

.adb.o:
	$(GNATGCC)  $(DO_NOT_LINK) -I$(ADABROKER_PATH) $<

vehicle.o : vehicle.ads vehicle.adb $(ADABROKER_PATH)

weapon.o : weapon.ads weapon.adb $(ADABROKER_PATH)

tank.o: vehicle.ads weapon.ads tank.ads tank.adb $(ADABROKER_PATH)

client.o: $(ADABROKER_PATH) .

######################################################

clean:
	rm *.ali *.o *~

