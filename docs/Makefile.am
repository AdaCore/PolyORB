info_TEXINFOS = polyorb_ug.texi polyorb_dg.texi
man_MANS = po_names.1 idlac.1 iac.1 polyorb-config.1 polyorb.7

SUFFIXES = .idl .ads .adb .texi .sed .txt

FIG2DEV=fig2dev

FIGFILES=\
  xe-arch.fig\
  full-ex.fig\
  corba-arch.fig

FIG_PDFFILES = $(FIGFILES:=.pdf)
FIG_EPSFILES = $(FIGFILES:=.eps)
FIG_PNGFILES = $(FIGFILES:=.png)
FIG_TXTFILES = $(FIGFILES:=.txt)

# Ada sources from GLADE UG
GLADE_ADAFILES=\
  check_pid.adb\
  acrrt.adb\
  acrrt.ads\
  acrrci.adb\
  acrrci.ads\
  acrmain.adb\
  rcibank.ads\
  terminal.ads\
  types.ads\
  rasbank.ads\
  racwbank.ads\
  mirrorbank.ads\
  mirrorbank.adb\
  bankclient.adb\
  term1client.adb\
  term2client.adb\
  racwbank.adb\
  newterminal.ads\
  stringarraystream.ads\
  stringarraystream.adb\
  sharedobjects.ads\
  storage.ads\
  common.ads\
  newworkers.ads\
  newnewworkers.ads\
  workercity.ads\
  factory.ads\
  newfactory.ads\
  internal.ads\
  rempkg1.ads\
  rempkg1.adb\
  rempkg2.ads\
  rempkg2.adb\
  remexcmain.adb\
  node1.ads\
  node1.adb\
  node2.ads\
  node2.adb\
  nondeterministic.adb\
  asynchronousrt.ads\
  asynchronousrci.ads\
  asynchronousmain.adb\
  genericrci.ads\
  rciinstantiation.ads\
  rciclient.adb\
  normalinstantiation.ads\
  new_integers.ads\
  new_integers.adb

$(GLADE_ADAFILES): glade-sources
	gnatchop -w $<

GLADE_TEXIFILES1= $(GLADE_ADAFILES:.ads=.texi)
GLADE_TEXIFILES = $(GLADE_TEXIFILES1:.adb=-body.texi)

polyorb_ug_TEXINFOS = polyorb_ug_ref.texi \
	polyorb-corba_p-corbaloc.texi \
	polyorb-corba_p-server_tools.texi \
	polyorb-corba_p-naming_tools.texi \
	polyorb-rtcorba_p-setup.texi \
	polyorb-setup-tasking-ravenscar.texi \
	echo.texi \
	echo-impl.texi \
	echo-impl-body.texi \
	server-body.texi \
	client-body.texi \
	dsa_client-body.texi \
	dsa_server.texi \
	dsa_server-body.texi \
	dsa_echo-cfg.texi \
	$(GLADE_TEXIFILES)

#
# Rules to build specific files for PolyORB User's Guide
# 

polyorb-corba_p-corbaloc.ads: $(top_srcdir)/src/corba/polyorb-corba_p-corbaloc.ads
	cp -f $< .

polyorb-corba_p-naming_tools.ads: $(top_srcdir)/idls/cos/naming/polyorb-corba_p-naming_tools.ads
	cp -f $< .

polyorb-corba_p-server_tools.ads: $(top_srcdir)/src/corba/polyorb-corba_p-server_tools.ads
	cp -f $< .

polyorb-rtcorba_p-setup.ads: $(top_srcdir)/src/corba/rtcorba/polyorb-rtcorba_p-setup.ads
	cp -f $< .

polyorb-setup-tasking-ravenscar.ads: $(top_srcdir)/src/polyorb-setup-tasking-ravenscar.ads
	cp -f $< .

echo.idl: $(top_srcdir)/examples/corba/echo/echo.idl
	cp -f $< .

echo-impl.ads: $(top_srcdir)/examples/corba/echo/echo-impl.ads
	cp -f $< .

echo-impl.adb: $(top_srcdir)/examples/corba/echo/echo-impl.adb
	cp -f $< .

client.adb: $(top_srcdir)/examples/corba/echo/client.adb
	cp -f $< .

server.adb: $(top_srcdir)/examples/corba/echo/server.adb
	cp -f $< .

dsa_client.adb: $(top_srcdir)/examples/dsa/echo/client.adb
	cp -f $< ./dsa_client.adb

dsa_server.ads: $(top_srcdir)/examples/dsa/echo/server.ads
	cp -f $< ./dsa_server.ads

dsa_server.adb: $(top_srcdir)/examples/dsa/echo/server.adb
	cp -f $< ./dsa_server.adb

dsa_echo-cfg.ads: $(top_srcdir)/examples/dsa/echo/echo.cfg
	cp -f $< ./dsa_echo-cfg.ads

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

polyorb_ug_ref.aux: polyorb_ug_ref.tex polyorb_ug.bib
	rm -f polyorb_ug_ref.bbl
	-tex '\nonstopmode \input $<'

#
# Generic rules to build the documentation in specific formats
#

# The PolyORB User's Guide texi file does not provide Up links in @node
# commands, but we want to force generation of HTML documentation anyway.
%.html: ${srcdir}/%.texi
	${MAKE} $(FIG_PNGFILES)
	-${MAKEINFO} -o $@ --force --html --number-sections $<
	cp *.png $@

%.txt: ${srcdir}/%.texi $(FIG_TXTFILES)
	-${MAKEINFO} -o $@ --ifinfo --number-sections $<

%.pdf: ${srcdir}/%.texi
	${MAKE} $(FIG_PDFFILES)
	-${TEXI2DVI} -o $@ --pdf $<

# Setting the environment variable TEX to "tex" is needed on Windows,
# to work around a problem with Cygwin.
# Otherwise, we get an error like this:
# ---! /var/lib/texmf/web2c/etex.fmt was written by pdfetex
# (Fatal format file error; I'm stymied)
# /usr/bin/texi2dvi: texinfo.tex appears to be broken, quitting.
#See: http://www.mail-archive.com/bug-automake@gnu.org/msg00546.html
%.dvi: ${srcdir}/%.texi $(FIG_EPSFILES)
	TEX=tex \
	TEXINPUTS="$(am__TEXINFO_TEX_DIR)$(PATH_SEPARATOR)$$TEXINPUTS" \
	MAKEINFO='$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) -I $(srcdir)' \
	$(TEXI2DVI) $<

#
# Texi generation from .ads files
#

ADA_KW    = use package is in out exception function with type constant \
		private of return range procedure begin end array record \
		subtype generic limited access all

%.texi: %.ads ada.sed $(top_builddir)/support/gentexifile
	AWK="${AWK}" SED="${SED}" $(top_builddir)/support/gentexifile $< NOGROUP
	-@${RM} genout

%-body.texi: %.adb ada.sed $(top_builddir)/support/gentexifile
	AWK="${AWK}" SED="${SED}" $(top_builddir)/support/gentexifile $< NOGROUP
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

%.texi: %.idl idl.sed $(top_builddir)/support/gentexifile
	AWK="${AWK}" SED="${SED}" $(top_builddir)/support/gentexifile $<
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
# Figures
#

%.fig.eps: %.fig
	$(FIG2DEV) -L eps $< $@

%.fig.pdf: %.fig
	$(FIG2DEV) -L pdf $< $@

%.fig.png: %.fig
	$(FIG2DEV) -L png $< $@

#
# Local rules
# 

all-local: MANIFEST

clean-local: maintainer-clean
	-@${RM} -f polyorb_ug.pdf polyorb_ug.txt polyorb_ug.info \
		polyorb_ug_ref.aux polyorb_ug_ref.bbl polyorb_ug_ref.blg \
		polyorb_ug_ref.dvi polyorb_ug_ref.log \
		${polyorb_ug_TEXINFOS} \
		polyorb_dg.pdf polyorb_dg.txt polyorb_dg.info \
		idl.sed ada.sed *.ads *.adb *.idl
	-@${RM} -fr polyorb_ug.html polyorb_dg.html polyorb_ug_toc.html

install-data-local:
	$(INSTALL) -d $(prefix)/share/doc/polyorb
	$(INSTALL) -d $(prefix)/share/doc/polyorb/info
	$(INSTALL) -d $(prefix)/share/doc/polyorb/ps
	$(INSTALL) -d $(prefix)/share/doc/polyorb/pdf
	$(INSTALL) -d $(prefix)/share/doc/polyorb/txt
	$(INSTALL) -d $(prefix)/share/doc/polyorb/html
	$(INSTALL) -d $(prefix)/share/doc/polyorb/html/polyorb_ug

	for fmt in info ps pdf txt; \
	do \
		$(INSTALL_DATA) polyorb_ug.$$fmt $(prefix)/share/doc/polyorb/$$fmt/; \
	done
	for f in polyorb_ug.html/*.html; \
	do \
		$(INSTALL_DATA) $$f $(prefix)/share/doc/polyorb/html/polyorb_ug; \
	done

MANIFEST: polyorb_ug.info polyorb_ug.html polyorb_ug.pdf polyorb_ug.ps polyorb_ug.txt \
	polyorb_ug.info polyorb_dg.html polyorb_dg.pdf polyorb_dg.txt polyorb_dg.ps
	-${RM} -f MANIFEST
	echo polyorb_ug.info >> MANIFEST
	echo polyorb_ug.ps >> MANIFEST
	echo polyorb_ug.pdf >> MANIFEST
	echo polyorb_ug.txt >> MANIFEST
	echo polyorb_ug.html/* | tr ' ' '\012' >> MANIFEST

release: all
	-${RM} -rf polyorb-doc
	mkdir -p polyorb-doc/polyorb_ug.html
	for f in `cat MANIFEST`; do \
		ln $$f polyorb-doc/$$f; \
	done
	ln MANIFEST polyorb-doc/MANIFEST
	tar cvzf polyorb-doc.tar.gz polyorb-doc
	-${RM} -rf polyorb-doc
