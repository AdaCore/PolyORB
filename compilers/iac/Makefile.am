SUBDIRS =

include $(srcdir)/../Makefile.common

PROG=iac

all-local: $(PROG)

PROG_FILES = nodes.ads nodes.adb

$(PROG_FILES): mknodes nodes.idl
	@-./mknodes nodes >nodes.ada
	gnatchop -w nodes.ada

mknodes : force
	$(GNATMAKE) -m -gnata -I. -I$(srcdir) $@\
		-cargs $(GNATFLAGS) -bargs $(BARGS)

$(PROG): force $(PROG_FILES)
	$(GNATMAKE) -m -gnata -I. -I$(srcdir) $@\
		-cargs $(GNATFLAGS) -bargs $(BARGS)

tests: $(PROG)
	(cd testsuite; $(MAKE) all)

CLEANFILES = *.o *.ali b~*.ads b~*.adb

DISTCLEANFILES = $(PROG) mknodes


force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(exec_prefix)/bin/$(PROG)
