include $(srcdir)/../Makefile.common

PROG=gnatprfh

all-local: $(PROG)

$(PROG): $(GNATFILES)
	$(GNATMAKE) -a -I. -I$(srcdir) g-pehama.adb -o $@ \
		-cargs $(GNATFLAGS) -bargs $(BARGS)

GNATFILES=\
$(srcdir)/g-pehage.adb\
$(srcdir)/g-pehage.ads\
$(srcdir)/g-pehama.adb\
$(srcdir)/g-perhas.ads

CLEANFILES = *.o *.ali b~*.ads b~*.adb

DISTCLEANFILES = $(GNATFILES) $(PROG)

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(exec_prefix)/bin/$(PROG)
