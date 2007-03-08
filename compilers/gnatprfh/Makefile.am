include $(srcdir)/../Makefile.common

PROG=gnatprfh

all-local: $(PROG)

$(PROG): force
	$(GNATMAKE) -m -I. -I$(srcdir) $@ $(GNATMAKE_FLAGS)

CLEANFILES = *.o *.ali b~*.ads b~*.adb

DISTCLEANFILES = $(PROG)

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(bindir)/$(PROG)
