privatelibdir = $(libdir)/adasockets

bin_SCRIPTS = adasockets-config

SOURCES = \
	sockets-constants.ads \
	sockets-link.ads \
	sockets-multicast.ads \
	sockets-multicast.adb \
	sockets-naming.ads \
	sockets-naming.adb \
	sockets-thin.ads \
	sockets-utils.ads \
	sockets-utils.adb \
	sockets.ads \
	sockets.adb

OBJECTS = \
	sockets-constants.o \
	sockets-link.o \
	sockets-multicast.o \
	sockets-naming.o \
	sockets-thin.o \
	sockets-utils.o \
	sockets.o

ALIS = $(OBJECTS:.o=.ali)

CLEANFILES = *.o *.ali
DISTCLEANFILES = b_split.c split constants_nodef

constants_nodef: constants.c config.h
	$(CC) -I. -o constants_nodef $(srcdir)/constants.c

sockets-constants.ads: $(srcdir)/constants.sh split constants_nodef
	$(SHELL) $(srcdir)/constants.sh Sockets.Constants $(srcdir)

split: $(srcdir)/split.adb
	$(GNATMAKE) -g -O2 $(srcdir)/split.adb

%.o %.ali: %.adb %.ads
	$(ADA) -c $(ADAFLAGS) $<

%.o %.ali: %.adb
	$(ADA) -c $(ADAFLAGS) $<

%.o %.ali: %.ads
	$(ADA) -c $(ADAFLAGS) $<

all: $(OBJECTS) $(ALI)

install-data-local: all
	rm -rf $(DESTDIR)$(privatelibdir)
	$(mkinstalldirs) $(DESTDIR)$(privatelibdir)
	cp -p $(OBJECTS) $(ALIS) $(DESTDIR)$(privatelibdir)
	for i in $(SOURCES); do \
	  if test -f $$i; then \
	    cp -p $$i $(DESTDIR)$(privatelibdir); \
	  else \
	    cp -p $(srcdir)/$$i $(DESTDIR)$(privatelibdir); \
	  fi \
	done

uninstall-local:
	rm -rf $(DESTDIR)$(privatelibdir)

depend:
	$(top_srcdir)/utils/mkdep
	$(MAKE) Makefile

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
sockets-constants.o: sockets.ads sockets-constants.ads
sockets-link.o: sockets.ads sockets-link.ads
sockets-multicast.o: sockets.ads sockets.adb sockets-constants.ads sockets-link.ads sockets-multicast.ads sockets-multicast.adb sockets-naming.ads sockets-thin.ads sockets-utils.ads
sockets-naming.o: sockets.ads sockets-constants.ads sockets-naming.ads sockets-naming.adb sockets-thin.ads
sockets-thin.o: sockets.ads sockets-thin.ads
sockets-utils.o: sockets.ads sockets-utils.ads sockets-utils.adb
sockets.o: sockets.ads sockets.adb sockets-constants.ads sockets-link.ads sockets-naming.ads sockets-thin.ads sockets-utils.ads
split.o: split.adb
