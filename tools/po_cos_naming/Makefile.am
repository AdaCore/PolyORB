include $(srcdir)/../Makefile.common

corba_exe=po_cos_naming po_cos_naming_shell
ir_exe=ir_ab_names

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) ir_ab_names $(DESTDIR)$(bindir)/ir_ab_names
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) po_cos_naming $(DESTDIR)$(bindir)/po_cos_naming
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) po_cos_naming_shell $(DESTDIR)$(bindir)/po_cos_naming_shell

$(corba_exe): cos-naming-stamp-gen

.PHONY: cos-naming-stamp-gen
cos-naming-stamp-gen:
	cd $(top_builddir)/cos/naming && $(MAKE) stamp-gen
