CLEANFILES = *.o *.ali sockets-constants.ads
DISTCLEANFILES = b_split.c b~split.ad? split constants_nodef

constants_nodef: constants.c config.h
	$(CC) -I. -o constants_nodef $(srcdir)/constants.c

sockets-constants.ads: $(srcdir)/constants.sh split constants_nodef
	$(SHELL) $(srcdir)/constants.sh Sockets.Constants $(srcdir)

split: $(srcdir)/split.adb
	$(GNATMAKE) -g -O2 $(srcdir)/split.adb

SUFFIXES = .adb .ads .o .ali

.adb.o .ads.o .adb.ali .ads.ali:
	$(ADA) -c $(ADAFLAGS) $<

