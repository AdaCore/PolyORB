CLEANFILES = *.o *.ali sockets-constants.ads
DISTCLEANFILES = b_split.c b~split.ad? split constants_nodef

lib_LTLIBRARIES = libadasockets.la
libadasockets_la_SOURCES = $(ADA_SOURCES)
libadasockets_la_LIBADD = $(ADA_OBJECTS:.o=.lo)
libadasockets_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

ADA_SOURCES = \
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

ADA_OBJECTS = \
	sockets-constants.o \
	sockets-link.o \
	sockets-multicast.o \
	sockets-naming.o \
	sockets-thin.o \
	sockets-utils.o \
	sockets.o

ALIS = $(ADA_OBJECTS:.o=.ali)

ADACOMPILER = ../../support/adacompiler

ADAFLAGS = -I. -I$(srcdir)

ADAFLAGS = -I. -I$(srcdir)

LINK = $(LIBTOOL) $(ADA) -o libadasockets.la

CLEANFILES = *.o *.ali
DISTCLEANFILES = b_split.c split constants_nodef

constants_nodef: constants.c config.h
	$(CC) -I. -o constants_nodef $(srcdir)/constants.c

sockets-constants.ads: $(srcdir)/constants.sh split constants_nodef
	$(SHELL) $(srcdir)/constants.sh Sockets.Constants $(srcdir)

split: $(srcdir)/split.adb
	$(GNATMAKE) -g -O2 $(srcdir)/split.adb

SUFFIXES = .adb .ads .lo .ali

.adb.lo .ads.lo .adb.ali .ads.ali:
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

install-data-local: all
	rm -rf $(DESTDIR)$(privatelibdir)
	$(mkinstalldirs) $(DESTDIR)$(privatelibdir)
	$(INSTALL) -m 444 $(ALIS) $(DESTDIR)$(privatelibdir)
	for i in $(ADA_SOURCES); do \
	  if test -f $$i; then \
	    $(INSTALL) -m 644 $$i $(DESTDIR)$(privatelibdir); \
	  else \
	    $(INSTALL) -m 644  $(srcdir)/$$i $(DESTDIR)$(privatelibdir); \
	  fi \
	done

uninstall-local:
	rm -rf $(DESTDIR)$(privatelibdir)

depend:
	$(top_srcdir)/support/mkdep $(srcdir)
	$(MAKE) Makefile

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
b~split.lo:b~split.adb b~split.ads 
sockets-constants.lo:sockets-constants.ads sockets.ads 
sockets-link.lo:sockets-link.ads sockets.ads 
sockets-multicast.lo:sockets-constants.ads sockets-link.ads sockets-multicast.adb sockets-multicast.ads sockets-naming.ads sockets-thin.ads sockets-utils.adb sockets-utils.ads sockets.adb sockets.ads 
sockets-naming.lo:sockets-constants.ads sockets-naming.adb sockets-naming.ads sockets-thin.ads sockets.ads 
sockets-thin.lo:sockets-thin.ads sockets.ads 
sockets-utils.lo:sockets-utils.adb sockets-utils.ads sockets.ads 
sockets.lo:sockets-constants.ads sockets-link.ads sockets-naming.ads sockets-thin.ads sockets-utils.adb sockets-utils.ads sockets.adb sockets.ads 
split.lo:split.adb 
