info_TEXINFOS = polyorb_ug.texi polyorb_dg.texi
man_MANS = ab_names.1 idlac.1 polyorb-config.1 polyorb.7

SUFFIXES = .idl .ads .adb .texi .sed .pdf .txt .html

polyorb_ug_TEXINFOS = polyorb_ug_ref.texi \
	polyorb-corba_p-server_tools.texi \
	polyorb-corba_p-naming_tools.texi \
	echo.texi \
	echo-impl-spec.texi \
	echo-impl-body.texi \
	server.texi \
	client.texi

#
# Rules to build specific files for PolyORB User's Guide
# 

polyorb-corba_p-server_tools.ads: ../src/corba/polyorb-corba_p-server_tools.ads
	cp ../src/corba/polyorb-corba_p-server_tools.ads .

polyorb-corba_p-naming_tools.ads: ../cos/naming/polyorb-corba_p-naming_tools.ads
	cp ../cos/naming/polyorb-corba_p-naming_tools.ads .

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

polyorb_ug_ref.aux: polyorb_ug.bib
	rm -f polyorb_ug_ref.bbl
	tex '\nonstopmode \input polyorb_ug_ref'

#
# Generic rules to build the documentation in specific formats
#

%.html: ${srcdir}/%.texi
	-${MAKEINFO} -o $@ --ifinfo --html --number-sections $<

%.txt: ${srcdir}/%.texi
	-${MAKEINFO} -o $@ --number-sections $<

%.pdf: ${srcdir}/%.texi
	-${TEXI2DVI} -o $@ --pdf $<

#
# Texi generation from .ads files
#

ADA_KW    = use package is in out exception function with type constant \
		private of return range procedure begin end array record \
		subtype generic limited access all

%.texi: %.ads ada.sed ../support/gentexifile
	../support/gentexifile $< .ads
	-@${RM} genout

%.texi: %.adb ada.sed ../support/gentexifile
	../support/gentexifile $< .adb
	-@${RM} genout

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

#
# Texi generation from .idl files
#

IDL_KW = module typedef struct sequence enum interface exception void in out raises

%.texi: %.idl idl.sed ../support/gentexifile
	../support/gentexifile $< .idl
	-@${RM} genout

idl.sed:
	echo "s/{/\@{/g" > idl.sed
	echo "s/}/\@}/g" >> idl.sed
	for kw in $(IDL_KW); do \
		echo "s/^\([^-]* \)$$kw/\1@b{$$kw}/g" >> idl.sed; \
		echo "s/^\( *\)$$kw /\1@b{$$kw} /g" >> idl.sed; \
		echo "s/^$$kw$$/@b{$$kw}/g" >> idl.sed; \
	done

#
# Local rules
# 

all-local: polyorb_ug.info polyorb_ug.html polyorb_ug.pdf polyorb_ug.ps polyorb_ug.txt \
	polyorb_ug.info polyorb_dg.html polyorb_dg.pdf polyorb_dg.txt polyorb_dg.ps

clean-local:
	-@${RM} polyorb_ug.pdf polyorb_ug.txt polyorb_ug.info 
	-@${RM} polyorb_ug_ref.aux polyorb_ug_ref.bbl polyorb_ug_ref.blg 
	-@${RM} polyorb_ug_ref.dvi polyorb_ug_ref.log
	-@${RM} ${polyorb_ug_TEXINFOS}
	-@${RM} polyorb_dg.pdf polyorb_dg.txt polyorb_dg.info
	-@${RM} idl.sed ada.sed
	-@${RM} -r polyorb_ug.html polyorb_dg.html

install-data-local:
	$(INSTALL) -d $(prefix)/doc/polyorb
	$(INSTALL) -d $(prefix)/doc/polyorb/html
	$(INSTALL) -d $(prefix)/doc/polyorb/html/polyorb_ug
	$(INSTALL) -d $(prefix)/doc/polyorb/ps
	$(INSTALL) -d $(prefix)/doc/polyorb/pdf
	$(INSTALL) -d $(prefix)/doc/polyorb/txt
	for f in `ls polyorb_ug.html/*.html`; do  $(INSTALL_DATA) $$f $(prefix)/doc/polyorb/html/polyorb_ug; done
	for f in `ls polyorb_ug.ps`; do  $(INSTALL_DATA) $$f $(prefix)/doc/polyorb/ps; done
	for f in `ls polyorb_ug.pdf`; do  $(INSTALL_DATA) $$f $(prefix)/doc/polyorb/pdf; done
	for f in `ls polyorb_ug.txt`; do  $(INSTALL_DATA) $$f $(prefix)/doc/polyorb/txt; done

release: all
	-@${RM} MANIFEST
	@echo polyorb_ug.info >> MANIFEST
	@echo polyorb_ug.ps >> MANIFEST
	@echo polyorb_ug.pdf >> MANIFEST
	@echo polyorb_ug.txt >> MANIFEST
	@echo polyorb_ug.html/* >> MANIFEST
