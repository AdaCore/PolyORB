EPSFILES=\
droopi-arch.eps\
droopi-func-addressing.eps\
droopi-func-transport.eps\
droopi-func-binding.eps\
droopi-func-representation.eps\
droopi-func-protocol.eps\
droopi-func-activation.eps\
droopi-func-dispatching.eps\
droopi-design.eps\
components.eps\
annotations.eps

SOURCES=

BIBFILES = interop.bib

BASE=architecture

all: $(BASE).ps

$(BASE).ps: $(BASE).dvi
	dvips -o $(BASE).ps $(BASE).dvi

$(BASE).dvi: $(BASE).tex $(EPSFILES) $(BIBFILES)
	latex $(BASE).tex
	bibtex $(BASE) || true
	latex $(BASE).tex
	latex $(BASE).tex

slides.dvi: slides.tex
	latex slides.tex
	latex slides.tex

${SOURCES}: src.ada
	gnatchop -w $<

%.idl.eps: %.idl
	a2ps -1 -B --border=no -Ecorba.ssh $< -o $<.tmp.ps
	ps2epsi $<.tmp.ps $@
	: ./adjustbb $@
	${RM} $<.tmp.ps

%.ads.eps: %.ads
	a2ps -1 -B --border=no -Eada $< -o $<.tmp.ps
	ps2epsi $<.tmp.ps $@
	: ./adjustbb $@
	${RM} $<.tmp.ps

%.adb.eps: %.adb
	a2ps -1 -B --border=no -Eada $< -o $<.tmp.ps
	ps2epsi $<.tmp.ps $@
	: ./adjustbb $@
	${RM} $<.tmp.ps

%.eps: %.dot
	dot -Tps $< | sed 's/^[0-9]*.*clip newpath$$/newpath/' > $<.tmp.ps
	ps2epsi $<.tmp.ps $@
	${RM} $<.tmp.ps

%.eps:  %.fig
#	fig2dev -L eps $< $<.tmp.eps
#	ps2epsi $<.tmp.ps $<.tmp.eps
#	if grep ^Landscape $< ;\
#	then ./pstoportrait < $<.tmp.eps > $@ ;\
#	else mv $<.tmp.eps $@ ;\
#	fi
#	${RM} $<.tmp.*
	(unset LANG;unset LC_NUMERIC;unset LC_ALL;fig2dev -L eps $< $@)

%.eps: %.dia
	dia -e $@ $<

clean:
	rm -f $(FIGURES) *.aux *.log $(SOURCES) *.dvi *.ps *.eps *.bbl *.blg
