GNATGCC = gnatgcc
DO_NOT_LINK = -c

ADABROKER_INCLUDES = -I../../InterfaceORB

main: echo.o echo-proxies.o echo-impl.o echo-skeleton.o server.o client.o 

clean:
	rm *.ali *.o

.SUFFIXES: .adb .o

.adb.o:
	$(GNATGCC)  $(DO_NOT_LINK) $(ADABROKER_INCLUDES) $<

echo.o : echo.ads echo.adb echo-proxies.ads echo-proxies.adb 

echo-impl.o : echo-impl.ads echo-impl.adb echo.o

echo-proxies.o: echo-proxies.ads echo-proxies.adb echo.o

echo-skeleton.o: echo-skeleton.ads echo-skeleton.adb echo.o