SUBDIRS =

include $(srcdir)/../Makefile.common

PROG=iac

all-local: $(PROG)

PROG_FILES = frontend-nodes.ada backend-be_corba_ada-nodes.ada

INCLUDES= -I. -I$(srcdir) -I$(srcdir)/../common_files

%.ada:%.idl mknodes
	./mknodes $< >$@
	gnatchop -w $@

mknodes : force 
	$(GNATMAKE) -m $(INCLUDES) $@ $(GNATMAKE_FLAGS)

$(PROG): force mknodes $(PROG_FILES)
	$(GNATMAKE) -m $(INCLUDES) $(PROG) $(GNATMAKE_FLAGS)

tests: $(PROG)
	(cd testsuite; $(MAKE) all)

CLEANFILES = *.o *.ali b~*.ads b~*.adb *~ backend-be_corba_ada-nodes.ad? \
             frontend-nodes.ad?

DISTCLEANFILES = $(PROG) mknodes

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(exec_prefix)/bin/$(PROG)
	cd $(DESTDIR)$(exec_prefix)/bin && $(RM) -f idlac && $(LN_S) $(PROG) idlac
