include $(srcdir)/../Makefile.common

GNATMAKE_FLAGS = -I$(srcdir) \
	-I../../src \
	-I$(srcdir)/../../src \
	-I../../adasockets/src \
	-I$(srcdir)/../../adasockets/src \
	-g -O2 -m -gnata

PROGS= ab_names test_naming

bin_PROGS = ab_names

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

CLEANFILES += $(GEN_1) $(GEN_2)

$(GEN_1): $(IDL_1) $(IDLAC)
	$(IDLAC) $(IDL_1)

$(GEN_2): $(IDL_2) $(IDLAC)
	$(IDLAC) $(IDL_2)

test_naming: force $(GEN_1) $(GEN_2) $(IMP_1) $(IMP_2)
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)

ab_names: force $(GEN_1) $(GEN_2) $(IMP_1) $(IMP_2) ab_names.adb
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)
