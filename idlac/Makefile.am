SUBDIRS = testsuite

all-local: testparser testlexer testgen idlac

testparser testlexer testgen idlac: force $(TREE_FILES)
	$(GNATMAKE) -I. -I$(srcdir) $(GNATFLAGS) $@

TREE_FILES=idl_fe-tree.ads idl_fe-tree.adb

$(TREE_FILES): $(srcdir)/nodes.txt $(srcdir)/make_nodes.py
	$(PYTHON) $(srcdir)/make_nodes.py $(srcdir)/nodes.txt > idl_fe-tree.ada
	$(GNATCHOP) -w idl_fe-tree.ada
	$(RM) idl_fe-tree.ada

autoconf:
	python make_nodes.py nodes.txt > xxx
	gnatchop -w xxx
	rm -f xxx

CLEANFILES = *.o *.ali b~*.ads b~*.adb

DISTCLEANFILES = $(TREE_FILES) testparser testlexer testgen idlac

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) idlac $(DESTDIR)$(exec_prefix)/bin/idlac
