AUTOMAKE_OPTIONS = no-dependencies

include $(srcdir)/../Makefile.common

if ENABLE_IR
FORCE_IR=		force-ir
IR_AB_NAMES=		ir_ab_names
IR_GNATMAKE_FLAGS=	-I../ir -I$(srcdir)/../ir
endif
FORCE=	force $(FORCE_IR)

PROGS= ab_names test_naming $(IR_AB_NAMES)
TOOLS=	polyorb-corba_p-naming_tools.adb
EXTRA=	$(TOOLS:.adb=.ali)

COS_DIRS=../../idls/cos/naming
COS_GNATMAKE_FLAGS=-I../../idls/cos/naming

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
	$(IDLAC) $(IDL)
	touch stamp-gen

ab_names test_naming: $(FORCE) $(GEN)
	$(GNATMAKE) $@ $(GNATMAKE_FLAGS)

ir_ab_names: $(FORCE) $(GEN)
	$(GNATMAKE) $@ $(IR_GNATMAKE_FLAGS) $(GNATMAKE_FLAGS)
	
force-ir:
	cd ../ir && $(MAKE)

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) ab_names $(DESTDIR)$(exec_prefix)/bin/ab_names
