AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../support

GNAT_STYLE= -gnaty3bcefhikmt
GNATMAKE_FLAGS= $(GNAT_STYLE) -O2 -g -gnatf -gnatwu -gnatwe -gnatgao

all-local: testlexer testparser testgen idlac

testparser testlexer testgen idlac: force $(TREE_FILES)
	$(GNATMAKE) $(GNATMAKE_FLAGS) $@

TREE_FILES=idl_fe-tree.ads idl_fe-tree.adb

$(TREE_FILES): nodes.txt make_nodes.py
	$(PYTHON) make_nodes.py > idl_fe-tree.ada
	$(GNATCHOP) -w idl_fe-tree.ada
	rm idl_fe-tree.ada

DISTCLEANFILES = $(TREE_FILES)

force::
