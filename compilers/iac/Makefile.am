SUBDIRS =

include $(srcdir)/../Makefile.common

PROG=idlac
MAIN=iac.adb

all-local: $(PROG)

PROG_FILES = frontend-nodes.ada backend-be_corba_ada-nodes.ada

includes = -I. -I$(srcdir) -I../common_files

%.ada:%.idl mknodes
	./mknodes $< >$@
	gnatchop -w $@

mknodes : force 
	$(GNATMAKE) -m $(includes) $@ $(GNATMAKE_FLAGS)

$(PROG): force mknodes $(PROG_FILES)
	$(GNATMAKE) -m $(includes) -o $@ $(MAIN) $(GNATMAKE_FLAGS)

tests: $(PROG)
	(cd testsuite; $(MAKE) all)

CLEANFILES = *.o *.ali b~*.ads b~*.adb *~ backend-be_corba_ada-nodes.ad? \
             frontend-nodes.ad?

DISTCLEANFILES = $(PROG) mknodes

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(exec_prefix)/bin/$(PROG)
