GNATGCC = gnatgcc
DO_NOT_LINK = -c
BIND = gnatbind
LINK = gnatlink

ADABROKER_INCLUDES = ../../InterfaceORB

client: vehicle.o weapon.o tank.o client.o 
	$(BIND) -I$(ADABROKER_INCLUDES) client.ali
	$(LINK)  client.ali

clean:
	rm *.ali *.o

.SUFFIXES: .adb .o

.adb.o:
	$(GNATGCC)  $(DO_NOT_LINK) -I$(ADABROKER_INCLUDES) $<

vehicle.o : vehicle.ads vehicle.adb $(ADABROKER_INCLUDES)

weapon.o : weapon.ads weapon.adb $(ADABROKER_INCLUDES)

tank.o: vehicle.ads weapon.ads tank.ads tank.adb $(ADABROKER_INCLUDES)

client.o: $(ADABROKER_INCLUDES) .

