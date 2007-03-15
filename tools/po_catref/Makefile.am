include $(srcdir)/../Makefile.common

.PHONY: force
FORCE_GEN_CODESET=force

GEN_FILES=polyorb-giop_p-code_sets-description_data.ads

poly_exe=po_catref

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) po_catref $(DESTDIR)$(bindir)/po_catref

polyorb-giop_p-code_sets-description_data.ads: $(GEN_CODESET) $(top_srcdir)/src/giop/cs_registry1.2h Makefile
	$(GEN_CODESET) -d "PolyORB.GIOP_P.Code_Sets.Description_Data" \
	  polyorb-giop_p-code_sets-description_data.ads.new \
	  < $(top_srcdir)/src/giop/cs_registry1.2h 
	$(MOVEIFCHANGE) polyorb-giop_p-code_sets-description_data.ads.new polyorb-giop_p-code_sets-description_data.ads
