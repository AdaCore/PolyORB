AUTOMAKE_OPTIONS = no-dependencies

include $(srcdir)/../Makefile.common

PROGS= ab_names test_naming

COS_DIRS=../../idls/cos/naming
COS_IMPL=\
  cosnaming-bindingiterator-impl.adb \
  cosnaming-bindingiterator-impl.ads \
  cosnaming-namingcontext-impl.adb \
  cosnaming-namingcontext-impl.ads

IDL=$(srcdir)/File.idl

GEN=\
file-helper.adb \
file-helper.ads \
file-skel.adb \
file-skel.ads \
file-stream.adb \
file-stream.ads \
file.adb \
file.ads \
file_idl_file.ads

IMPL=\
file-impl.adb \
file-impl.ads

CLEANFILES += $(GEN) stamp-gen

$(GEN): stamp-gen
stamp-gen: $(IDL) $(IDLAC)
	touch stamp-gen
	$(IDLAC) $(IDL)

test_naming: force $(GEN) $(IMPL) $(COS_IMPL)
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)

ab_names: force $(GEN) $(IMPL) $(COS_IMPL) ab_names.adb
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)

naming_tools.o:
	$(GNATMAKE) -c naming_tools $(GNATMAKE_FLAGS)

naming_tools.o: naming_tools.ads naming_tools.adb cosnaming.ads
naming_tools.o: cosnaming-namingcontext.ads cosnaming-namingcontext-helper.ads

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) ab_names $(DESTDIR)$(exec_prefix)/bin/ab_names
