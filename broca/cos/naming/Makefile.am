IDLAC = ../../idlac/idlac
SOCK_CONS = ../../adasockets/src/sockets-constants.ads

GNATMAKE_FLAGS = -I$(srcdir) \
	-I../../src \
	-I$(srcdir)/../../src \
	-I../../adasockets/src \
	-I$(srcdir)/../../adasockets/src \
	-g -O2 -m -gnata

E1=file
E2=cosnaming
E2_1=cosnaming-namingcontext
E2_2=cosnaming-bindingiterator

GENERATED_FILES = \
        $(E1)_idl_file.ads $(E1)_idl_file.adb \
        $(E1).ads $(E1).adb \
        $(E1)-skel.ads $(E1)-skel.adb \
        $(E1)-stream.ads $(E1)-stream.adb \
        $(E1)-helper.ads $(E1)-helper.adb \
        $(E2)_idl_file.ads $(E2)_idl_file.adb \
        $(E2).ads $(E2).adb \
        $(E2)-skel.ads $(E2)-skel.adb \
        $(E2)-stream.ads $(E2)-stream.adb \
        $(E2)-helper.ads $(E2)-helper.adb \
        $(E2_1).ads $(E2_1).adb \
        $(E2_1)-skel.ads $(E2_1)-skel.adb \
        $(E2_1)-stream.ads $(E2_1)-stream.adb \
        $(E2_1)-helper.ads $(E2_1)-helper.adb \
        $(E2_2).ads $(E2_2).adb \
        $(E2_2)-skel.ads $(E2_2)-skel.adb \
        $(E2_2)-stream.ads $(E2_2)-stream.adb \
        $(E2_2)-helper.ads $(E2_2)-helper.adb  

CLEANFILES = b~*.* *.o *.ali $(GENERATED_FILES) test_naming

DISTCLEANFILES = test_naming

all-local: test_naming

SUFFIXES = .idl .ads .adb

$(GENERATED_FILES): $(E1).idl $(E2).idl $(IDLAC)

.idl.ads:
	$(IDLAC) $<

.idl.adb:
	$(IDLAC) $<

$(IDLAC): force
	cd ../../idlac && $(MAKE) idlac

$(SOCK_CONS):
	cd ../../adasockets/src && $(MAKE) sockets-constants.ads

test_naming: force $(GENERATED_FILES)
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)

force::
	cd ../../src && $(MAKE)
