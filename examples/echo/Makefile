GNATGCC = gnatgcc
DO_NOT_LINK = -c

ADABROKER_INCLUDES = -I../../InterfaceORB

main: echo.o echo-proxies.o echo-impl.o echo-skeleton.o server.o client.o 

clean:
	rm *.ali *.o

.SUFFIXES: .adb .o

.adb.o:
	$(GNATGCC)  $(DO_NOT_LINK) $(ADABROKER_INCLUDES) $<

