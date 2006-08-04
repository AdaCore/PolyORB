include $(srcdir)/../Makefile.common

.PHONY: force

PROG=po_gnatdist

all-local: $(PROG)
	
xe_sysdep.adb: xe_sysdep-std.adb xe_sysdep-nt.adb
	@echo Selecting xe_sysdep.adb
	@if test -z "${WINDOWS_TRUE}"; then \
	   cp $(srcdir)/xe_sysdep-nt.adb xe_sysdep.adb; \
	else \
	   cp $(srcdir)/xe_sysdep-std.adb xe_sysdep.adb; \
	fi

$(PROG): xe_sysdep.adb force
	$(GNATMAKE) -I. -I$(srcdir) $(srcdir)/xe_main.adb -o $(PROG) $(GNATMAKE_FLAGS)

CLEANFILES = b~*.ad[bs] *.ali xe_sysdep.adb

DISTCLEANFILES = $(PROG)

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(bindir)/$(PROG)
