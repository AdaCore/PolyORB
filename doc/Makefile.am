info_TEXINFOS = polyorb_ug.texi
man_MANS = ab_names.1 idlac.1 polyorb-config.1 polyorb.7

SUFFIXES = .ads .texi

polyorb_ug_TEXINFOS = polyorb_ug_ref.texi \
	../src/corba/polyorb-corba_p-server_tools.texi \
	../cos/naming/polyorb-corba_p-naming_tools.texi

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

ADA_KW    = use package is in out exception function with type constant \
		private of return range procedure begin end array record \
		subtype generic limited access all

.ads.texi: ada.sed ../support/gentexifile
	../support/gentexifile $< .ads

ada.sed:
	echo "s/\([^@]\)@\([^@]\)/\1@@\2/g" > ada.sed
	echo "s/-- \(.*\)$$/-- @i{\1}/" >> ada.sed
	echo "s/\([^-][^-][^\"]*\)\"\([^\"]*\)\"/\1\"@i{\2}\"/g" >> ada.sed
	echo "s/@@/@@@@/g" >> ada.sed
	echo "s/_@_/_@@_/g" >> ada.sed
	for kw in $(ADA_KW); do \
		echo "s/^\([^-]* \)$$kw/\1@b{$$kw}/g" >> ada.sed; \
		echo "s/^\( *\)$$kw /\1@b{$$kw} /g" >> ada.sed; \
		echo "s/^$$kw$$/@b{$$kw}/g" >> ada.sed; \
	done

all-local: polyorb_ug.html polyorb_ug.pdf polyorb_ug.ps polyorb_ug.txt

clean-local:
	-@${RM} polyorb_ug.pdf polyorb_ug.txt polyorb_ug.info polyorb_ug*.html 
	-@${RM} polyorb_ug_ref.aux polyorb_ug_ref.bbl polyorb_ug_ref.blg 
	-@${RM} polyorb_ug_ref.dvi polyorb_ug_ref.log polyorb_ug_ref.texi
	-@${RM} ${polyorb_ug_TEXINFOS}

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
