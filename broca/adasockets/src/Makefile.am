CLEANFILES = *.o *.ali sockets-constants.ads
DISTCLEANFILES = b_split.c b~split.ad? split constants_nodef

constants_nodef: constants.c config.h
	$(CC) -I. -o constants_nodef $(srcdir)/constants.c

sockets-constants.ads: $(srcdir)/constants.sh split constants_nodef
	$(SHELL) $(srcdir)/constants.sh Sockets.Constants $(srcdir)

split: $(srcdir)/split.adb
	$(GNATMAKE) -g -O2 $(srcdir)/split.adb

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
	$(top_srcdir)/utils/mkdep $(srcdir)
	$(MAKE) Makefile

SUFFIXES = .adb .ads .o .ali

.adb.o .ads.o .adb.ali .ads.ali:
	$(ADA) -c $(ADAFLAGS) $<

