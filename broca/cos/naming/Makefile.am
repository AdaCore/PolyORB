IDLAC = ../../idlac/idlac
SOCK_CONS = ../../adasockets/src/sockets-constants.ads

GNATMAKE_FLAGS = -I$(srcdir) \
	-I../../src \
	-I$(srcdir)/../../src \
	-I../../adasockets/src \
	-I$(srcdir)/../../adasockets/src \
	-g -O2 -m -gnata

IDL_1=File
IDL_2=CosNaming

ADA_1=file
ADA_2=cosnaming
ADA_2_1=cosnaming-namingcontext
ADA_2_2=cosnaming-bindingiterator

GEN_1 = \
        $(ADA_1)_idl_file.ads $(ADA_1)_idl_file.adb \
        $(ADA_1).ads $(ADA_1).adb \
        $(ADA_1)-skel.ads $(ADA_1)-skel.adb \
        $(ADA_1)-stream.ads $(ADA_1)-stream.adb \
        $(ADA_1)-helper.ads $(ADA_1)-helper.adb

GEN_2 = \
        $(ADA_2)_idl_file.ads $(ADA_2)_idl_file.adb \
        $(ADA_2).ads $(ADA_2).adb \
        $(ADA_2)-skel.ads $(ADA_2)-skel.adb \
        $(ADA_2)-stream.ads $(ADA_2)-stream.adb \
        $(ADA_2)-helper.ads $(ADA_2)-helper.adb \
        $(ADA_2_1).ads $(ADA_2_1).adb \
        $(ADA_2_1)-skel.ads $(ADA_2_1)-skel.adb \
        $(ADA_2_1)-stream.ads $(ADA_2_1)-stream.adb \
        $(ADA_2_1)-helper.ads $(ADA_2_1)-helper.adb \
        $(ADA_2_2).ads $(ADA_2_2).adb \
        $(ADA_2_2)-skel.ads $(ADA_2_2)-skel.adb \
        $(ADA_2_2)-stream.ads $(ADA_2_2)-stream.adb \
        $(ADA_2_2)-helper.ads $(ADA_2_2)-helper.adb  

IMP_1 = $(ADA_1)-impl.ads $(ADA_1)-impl.adb

IMP_2 = $(ADA_2_1)-impl.ads $(ADA_2_1)-impl.adb \
	$(ADA_2_2)-impl.ads $(ADA_2_2)-impl.adb 

CLEANFILES = b~*.* *.o *.ali $(GEN_1) $(GEN_2) $(IMP_1) $(IMP_2) test_naming

DISTCLEANFILES = test_naming

all-local: test_naming

SUFFIXES = .idl .ads .adb

$(IMP_1): $(IDL_1).idl $(IDLAC)
	$(IDLAC) -i $(IDL_1).idl

$(IMP_2): $(IDL_2).idl $(IDLAC)
	$(IDLAC) -i $(IDL_2).idl

$(GEN_1): $(IDL_1).idl $(IDLAC)
	$(IDLAC) $(IDL_1).idl

$(GEN_2): $(IDL_2).idl $(IDLAC)
	$(IDLAC) $(IDL_2).idl

$(IDLAC): force
	cd ../../idlac && $(MAKE) idlac

$(SOCK_CONS):
	cd ../../adasockets/src && $(MAKE) sockets-constants.ads

test_naming: force $(GEN_1) $(GEN_2)
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)

force::
	cd ../../src && $(MAKE)
