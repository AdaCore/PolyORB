SUBDIRS = testsuite

GNAT_STYLE= -gnaty3bcefhikmt
GNATMAKE_FLAGS= $(GNAT_STYLE) -O2 -g -gnatf -gnatwu -gnatwe -gnatgao

all-local: testlexer testparser testexpansion testgen idlac

testparser testlexer testexpansion testgen idlac: force $(TREE_FILES)
	$(GNATMAKE) $(GNATMAKE_FLAGS) $@

TREE_FILES=idl_fe-tree.ads idl_fe-tree.adb

$(TREE_FILES): nodes.txt make_nodes.py
	$(PYTHON) make_nodes.py > idl_fe-tree.ada
	$(GNATCHOP) -w idl_fe-tree.ada
	rm idl_fe-tree.ada

DISTCLEANFILES = $(TREE_FILES)

force::
