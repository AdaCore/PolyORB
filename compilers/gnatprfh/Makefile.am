include $(srcdir)/../Makefile.common

PROG=gnatprfh

all-local: $(PROG)

$(PROG): $(GNATFILES)
	$(GNATMAKE) -a -m -I. -I$(srcdir) g-pehama.adb -o $@
#          -cargs $(GNATFLAGS) -bargs $(BARGS) 
# WAG:3.15 we must compile this executable using specific flags.

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
