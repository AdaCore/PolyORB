IDLAC = ../../idlac/idlac
SOCK_CONS = ../../adasockets/src/sockets-constants.ads

GNATMAKE_FLAGS = -I$(srcdir) \
	-I../../src \
	-I$(srcdir)/../../src \
	-I../../adasockets/src \
	-I$(srcdir)/../../adasockets/src \
	-g -O2 -m -gnata

PROGRAM=test_naming

IDL_1=$(srcdir)/File.idl
IDL_2=$(srcdir)/CosNaming.idl

GEN_1=\
file-helper.adb \
file-helper.ads \
file-skel.adb \
file-skel.ads \
file-stream.adb \
file-stream.ads \
file.adb \
file.ads \
file_idl_file.ads

GEN_2=\
cosnaming-bindingiterator-helper.adb \
cosnaming-bindingiterator-helper.ads \
cosnaming-bindingiterator-skel.adb \
cosnaming-bindingiterator-skel.ads \
cosnaming-bindingiterator-stream.adb \
cosnaming-bindingiterator-stream.ads \
cosnaming-bindingiterator.adb \
cosnaming-bindingiterator.ads \
cosnaming-helper.adb \
cosnaming-helper.ads \
cosnaming-namingcontext-helper.adb \
cosnaming-namingcontext-helper.ads \
cosnaming-namingcontext-skel.adb \
cosnaming-namingcontext-skel.ads \
cosnaming-namingcontext-stream.adb \
cosnaming-namingcontext-stream.ads \
cosnaming-namingcontext.adb \
cosnaming-namingcontext.ads \
cosnaming-stream.adb \
cosnaming-stream.ads \
cosnaming.ads \
cosnaming_idl_file.ads

IMP_1=\
file-impl.adb \
file-impl.ads

IMP_2=\
cosnaming-namingcontext-impl.adb \
cosnaming-namingcontext-impl.ads \
cosnaming-bindingiterator-impl.adb \
cosnaming-bindingiterator-impl.ads

CLEANFILES = b~*.* *.o *.ali $(GEN_1) $(GEN_2) $(PROGRAM)

DISTCLEANFILES = $(PROGRAM)

all-local: $(PROGRAM)

SUFFIXES = .idl .ads .adb

$(GEN_1): $(IDL_1) $(IDLAC)
	$(IDLAC) $(IDL_1)

$(GEN_2): $(IDL_2) $(IDLAC)
	$(IDLAC) $(IDL_2)

$(IDLAC): force
	cd ../../idlac && $(MAKE) idlac

$(SOCK_CONS):
	cd ../../adasockets/src && $(MAKE) sockets-constants.ads

$(PROGRAM): force $(GEN_1) $(GEN_2) $(IMP_1) $(IMP_2)
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)

force::
	cd ../../src && $(MAKE)
