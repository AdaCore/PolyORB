info_TEXINFOS = polyorb_ug.texi
man_MANS = ab_names.1 idlac.1 polyorb-config.1 polyorb.7

all-local: polyorb_ug.html polyorb_ug.pdf polyorb_ug.ps polyorb_ug.txt

polyorb_ug_TEXINFOS = polyorb_ug_ref.texi

polyorb_ug.html: ${srcdir}/polyorb_ug.texi
	-${TEXI2HTML} -menu -split_chapter -number $<

polyorb_ug.pdf: ${srcdir}/polyorb_ug.texi
	-${TEXI2DVI} --pdf $<

polyorb_ug.txt: ${srcdir}/polyorb_ug.texi
	-${MAKEINFO} -o $@ --number-sections $<

polyorb_ug_ref.texi: polyorb_ug_ref.bbl
	@${SED} -e 's/~/ /g' \
	    -e 's/\\[-`'\''"c^]//g' \
	    -e 's/\\emdash */---/g' \
	    -e 's/metapost/MetaPost/g' \
	    -e 's/\\MF/Metafont/g' \
	    -e 's/\\METAFONT/METAFONT/g' \
	    -e 's/\\TUB/TUGboat/g' \
	    -e 's/\\LaTeX/La@TeX/g' \
	    -e 's/\\AMSTEX/AMS@TeX/g' \
	    -e 's/\\AmSTeX/AMS@TeX/g' \
	    -e 's/\\TeX/@TeX/g' \
	    -e 's/\\noopsort{[^}]*}//g' \
	    -e 's/[{}\]//g' \
	    -e 's/@\[/{/g' \
	    -e 's/@\]/}/g' \
	    -e 's/@TeX/@TeX{}/g' \
	$< >$@

polyorb_ug_ref.bbl: polyorb_ug_ref.aux
	-bibtex polyorb_ug_ref

polyorb_ug_ref.aux: polyorb_ug_ref.tex
	rm -f polyorb_ug_ref.bbl
	tex '\nonstopmode \input polyorb_ug_ref'

clean-local:
	-@${RM} polyorb_ug.pdf polyorb_ug.txt polyorb_ug.info polyorb_ug*.html 
	-@${RM} polyorb_ug_ref.aux polyorb_ug_ref.bbl polyorb_ug_ref.blg 
	-@${RM} polyorb_ug_ref.dvi polyorb_ug_ref.log polyorb_ug_ref.texi

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
