SUBDIRS = testsuite

if DEBUG
TESTPROGS = testparser testlexer testgen
else
TESTPROGS =
endif

all-local: $(TESTPROGS) idlac

testparser testlexer testgen idlac: force $(TREE_FILES)
	$(GNATMAKE) -m -I. -I$(srcdir) $(GNATFLAGS) $@

TREE_FILES=idl_fe-tree.ads idl_fe-tree.adb

$(TREE_FILES): $(srcdir)/nodes.txt $(srcdir)/make_nodes.py
	$(PYTHON) $(srcdir)/make_nodes.py $(srcdir)/nodes.txt > idl_fe-tree.ada
	$(GNATCHOP) -w idl_fe-tree.ada
	$(RM) idl_fe-tree.ada

CLEANFILES = *.o *.ali b~*.ads b~*.adb

DISTCLEANFILES = $(TREE_FILES) testparser testlexer testgen idlac

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) idlac $(DESTDIR)$(exec_prefix)/bin/idlac
