SUBDIRS = testsuite

GNAT_STYLE= -gnaty3bcefhikmt
GNATMAKE_FLAGS= $(GNAT_STYLE) -O2 -g -gnatf -gnatwu -gnatwe -gnatgao

all-local: testparser testlexer testexpansion testgen idlac

testparser testlexer testexpansion testgen idlac: force $(TREE_FILES)
	$(GNATMAKE) -I. -I$(srcdir) $(GNATMAKE_FLAGS) $@

TREE_FILES=idl_fe-tree.ads idl_fe-tree.adb

$(TREE_FILES): $(srcdir)/nodes.txt $(srcdir)/make_nodes.py
	$(PYTHON) $(srcdir)/make_nodes.py $(srcdir)/nodes.txt > idl_fe-tree.ada
	$(GNATCHOP) -w idl_fe-tree.ada
	$(RM) idl_fe-tree.ada

CLEANFILES = *.o *.ali b~*.ads b~*.adb

DISTCLEANFILES = $(TREE_FILES) testparser testlexer testexpansion testgen idlac

force::

install-exec-local:
	$(INSTALL) -d $(exec_prefix)/bin
	$(INSTALL_PROGRAM) idlac $(exec_prefix)/bin/idlac

uninstall-local:
	$(RM) $(exec_prefix)/bin/idlac
