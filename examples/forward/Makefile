CC = gnatgcc -c
IPATH = -I../../InterfaceORB
CCMAKE = gnatmake

.SUFFIXES:
	.adb .o
	.ads .ali

.adb.o:
	$(CC) $(IPATH) $<

.ads.ali:
	$(CC) $(IPATH) $<


client:
	$(CCMAKE) $(IPATH) client.adb

