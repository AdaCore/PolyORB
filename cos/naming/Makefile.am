AUTOMAKE_OPTIONS = no-dependencies

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/corba/cos

corba_exe=po_cos_naming po_cos_naming_shell
ir_exe=ir_ab_names

PROGS= @APPLI_EXES@ @SERVICE_EXES@

FORCE=	force

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
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) po_cos_naming $(DESTDIR)$(bindir)/po_cos_naming
	$(INSTALL_PROGRAM) po_cos_naming_shell $(DESTDIR)$(bindir)/po_cos_naming_shell
	for f in $(srcdir)/cos*.ad[bs] cos*.ali polyorb-corba_p-naming_tools.ad[bs] polyorb-corba_p-naming_tools.ali ; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done

$(ir_exe): force_ir

force_ir: force
	cd ../ir && $(MAKE)
