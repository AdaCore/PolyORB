GNAT_STYLE=-gnaty3bcefhikmt
GNATMAKE_FLAGS= $(GNAT_STYLE) -gnatf -gnatwu -gnatwe -gnatgao
all: testlexer testparser idlac # gidlac # idlac

testparser testlexer gidlac idlac: force
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
