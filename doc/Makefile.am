info_TEXINFOS = polyorb_ug.texi

all-local: polyorb_ug.html polyorb_ug.pdf polyorb_ug.ps polyorb_ug.txt

polyorb_ug.html: ${srcdir}/polyorb_ug.texi
	-${TEXI2HTML} -menu -split_chapter -number $<

polyorb_ug.pdf: ${srcdir}/polyorb_ug.texi
	-${TEXI2DVI} --pdf $<

polyorb_ug.txt: ${srcdir}/polyorb_ug.texi
	-${MAKEINFO} -o $@ --number-sections $<

clean-local:
	-@${RM} polyorb_ug.pdf polyorb_ug.txt polyorb_ug.info polyorb_ug*.html 

install-data-local:
	$(INSTALL) -d $(prefix)/doc/polyorb
	$(INSTALL) -d $(prefix)/doc/polyorb/html
	$(INSTALL) -d $(prefix)/doc/polyorb/ps
	$(INSTALL) -d $(prefix)/doc/polyorb/pdf
	$(INSTALL) -d $(prefix)/doc/polyorb/txt
	for f in `ls polyorb_ug*.html`; do  $(INSTALL_DATA) $$f $(prefix)/doc/polyorb/html; done
	for f in `ls polyorb_ug.ps`; do  $(INSTALL_DATA) $$f $(prefix)/doc/polyorb/ps; done
	for f in `ls polyorb_ug.pdf`; do  $(INSTALL_DATA) $$f $(prefix)/doc/polyorb/pdf; done
	for f in `ls polyorb_ug.txt`; do  $(INSTALL_DATA) $$f $(prefix)/doc/polyorb/txt; done