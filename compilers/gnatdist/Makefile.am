include $(srcdir)/../Makefile.common

.PHONY: force

PROG=po_gnatdist

all-local: $(PROG)
	
$(PROG): force
	$(GNATMAKE) -I$(srcdir) -I. $(srcdir)/$(PROG).adb $(GNATMAKE_FLAGS)

CLEANFILES = b~*.ad[bs] *.o *.ali

DISTCLEANFILES = $(PROG)

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(bindir)/$(PROG)
