GNATGCC = gnatgcc
DO_NOT_LINK = -c

ADABROKER_INCLUDES = -I../../InterfaceORB

main: vehicle.o tank.o weapon.o

clean:
	rm *.ali *.o

.SUFFIXES: .adb .o

.adb.o:
	$(GNATGCC)  $(DO_NOT_LINK) $(ADABROKER_INCLUDES) $<

vehicle.o : vehicle.ads vehicle.adb

weapon.o : weapon.ads weapon.adb

tank.o: vehicle.ads weapon.ads tank.ads tank.adb