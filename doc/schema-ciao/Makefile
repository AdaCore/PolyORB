# Makefile for CIAO documentation.
# $Id: //droopi/main/doc/schema-ciao/Makefile#1 $

EPSFILES=\
ex-structure.ads.eps\
ex-structure.idl.eps\
sample.idl.eps \
unconstrained-array.idl.eps \
ex-variant_part.ads.eps\
ex-variant_part.idl.eps\
ex-racw.ads.eps\
ex-racw.idl.eps\
ciao.idl.eps

all: schema.ps

schema.ps: schema.dvi
	dvips -o $@ $^

schema.dvi: schema.tex $(EPSFILES)
	latex schema.tex
	bibtex schema
	latex schema.tex
	latex schema.tex

%.idl.eps: %.idl
	a2ps -1 -B --border=no -Ecorba.ssh --prologue=italic $< -o $<.tmp.ps
	ps2epsi $<.tmp.ps $@
#	./adjustbb $@
	${RM} $<.tmp.ps

%.ads.eps: %.ads
	a2ps -1 -B --border=no -Eada $< -o $<.tmp.ps
	ps2epsi $<.tmp.ps $@
#	./adjustbb $@
	${RM} $<.tmp.ps

%.adb.eps: %.adb
	a2ps -1 -B --border=no -Eada $< -o $<.tmp.ps
	ps2epsi $<.tmp.ps $@
#	./adjustbb $@
	${RM} $<.tmp.ps

clean:
	-rm -f $(FIGURES) *.aux *.log *.toc *.dvi *.ps *.eps *.bbl *.blg stamp-*

force:
