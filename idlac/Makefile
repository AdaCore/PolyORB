GNAT_STYLE=-gnaty3bcefhikmt
GNATMAKE_FLAGS= $(GNAT_STYLE) -g -gnatf -gnatwu -gnatwe -gnatgao
all: testlexer testparser #testgen idlac ada_be-expansion # gidlac # idlac

ACCESSOR_FILES=idl_fe-tree-accessors.ads idl_fe-tree-accessors.adb

$(ACCESSORS_FILES): idl_fe-tree.ads
	./mk_accessors.py

CONSTRUCTOR_FILES=idl_fe-tree-constructors.ads idl_fe-tree-constructors.adb

$(CONSTRUCTOR_FILES): idl_fe-tree.ads
	./mk_constructors.py


testparser testlexer testgen gidlac idlac ada_be-expansion : force #$(ACCESSOR_FILES) $(CONSTRUCTOR_FILES)
	gnatmake $(GNATMAKE_FLAGS) $@

TEST_FILES=\
all_exceptions/all_exceptions.idl \
all_functions/all_functions.idl \
all_types/all_types.idl

test:
	for i in $(TEST_FILES); do ./idlac < ../examples/$$i || exit 1; done

.PHONY: clean force

clean:
	$(RM) *.o *.ali *.c *~ idlac gidlac testlexer b~*

force:
