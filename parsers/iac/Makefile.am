SUBDIRS =

include $(srcdir)/../Makefile.common

PROG=iac

all-local: $(PROG)

PROG_FILES = nodes.ada backend-be_ada-nodes.ada

%.ada:%.idl
	@-./mknodes $< >$@
	gnatchop -w $@

mknodes : force 
	$(GNATMAKE) -m -gnata -I. -I$(srcdir) $@\
		-cargs $(GNATFLAGS) -bargs $(BARGS)

$(PROG): force mknodes $(PROG_FILES)
	$(GNATMAKE) -m -gnata -I. -I$(srcdir) $@\
		-cargs $(GNATFLAGS) -bargs $(BARGS)

tests: $(PROG)
	(cd testsuite; $(MAKE) all)

CLEANFILES = *.o *.ali b~*.ads b~*.adb *~

DISTCLEANFILES = $(PROG) mknodes


force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(exec_prefix)/bin/$(PROG)
