SUBDIRS =

include $(srcdir)/../Makefile.common

PROG=iac

all-local: $(PROG)

PROG_FILES = frontend-nodes.ada backend-be_ada-nodes.ada

includes = -I. -I$(srcdir) -I../common_files

%.ada:%.idl mknodes
	@-./mknodes $< >$@
	gnatchop -w $@

mknodes : force 
	$(GNATMAKE) -a -m -gnata -I. $(includes) $@\
		-cargs $(GNATFLAGS) -bargs $(BARGS)

$(PROG): force mknodes $(PROG_FILES)
	$(GNATMAKE) -a -m -gnata -I. $(includes) $@\
		-cargs $(GNATFLAGS) -bargs $(BARGS)

tests: $(PROG)
	(cd testsuite; $(MAKE) all)

CLEANFILES = *.o *.ali b~*.ads b~*.adb *~ backend-be_ada-nodes.ad? \
             frontend-nodes.ad?

DISTCLEANFILES = $(PROG) mknodes


force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(exec_prefix)/bin/$(PROG)
