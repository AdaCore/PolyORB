EPSFILES=corbaglobal.eps
PDFFILES=$(EPSFILES:.eps=.pdf)

whatis-adabroker.dvi: whatis-adabroker.tex $(EPSFILES)
	latex whatis-adabroker.tex
	latex whatis-adabroker.tex

whatis-adabroker.pdf: whatis-adabroker.tex $(PDFFILES)
	pdflatex whatis-adabroker.tex
	pdflatex whatis-adabroker.tex

%.eps:	%.fig
	fig2dev -L ps $< $<.tmp.ps
	ps2epsi $<.tmp.ps $@
	${RM} $<.tmp.*

%.pdf:	%.eps
	epstopdf $<

clean:
	rm -f $(EPSFILES) *.aux *.log *.ads *.adb *.dvi *.ps *.eps *.bbl *.blg
