include $(srcdir)/../Makefile.common

PROG=gnatprfh

all-local: $(PROG)

$(PROG): $(GNATFILES)
	$(GNATMAKE) -I. -I$(srcdir) g-pehama.adb -o $@ \
		-cargs $(GNATFLAGS) -bargs $(BARGS) 

# WAG:3.15 we use minimal recompilation flag '-m' to avoid recompiling
# GNAT runtime components.

GNATFILES=\
$(srcdir)/g-pehage.adb\
$(srcdir)/g-pehage.ads\
$(srcdir)/g-pehama.adb\
$(srcdir)/g-perhas.ads

CLEANFILES = *.o *.ali b~*.ads b~*.adb

DISTCLEANFILES = $(PROG)

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(exec_prefix)/bin/$(PROG)
