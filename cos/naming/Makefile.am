AUTOMAKE_OPTIONS = no-dependencies

include $(srcdir)/../Makefile.common

corba_exe=ab_names test_naming test_naming_corba
ir_exe=ir_ab_names
ir_dir=../ir

PROGS= po_names test_naming_generic @APPLI_EXES@ @SERVICE_EXES@

FORCE=	force

TOOLS=	polyorb-corba_p-naming_tools.adb
EXTRA=	$(TOOLS:.adb=.ali)

COS_DIRS=../../idls/cos/naming @SERVICE_DIRS@
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
file-ir_info.adb \
file-ir_info.ads \
file_idl_file-helper.adb \
file_idl_file-helper.ads \
file_idl_file-ir_info.adb \
file_idl_file-ir_info.ads \
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

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) ab_names $(DESTDIR)$(exec_prefix)/bin/ab_names