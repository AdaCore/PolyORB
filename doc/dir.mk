SLIDES=intro.fig client.fig exceptions.fig requests.fig \
 compilateur.fig conversion.fig poa-1.fig poa-2.fig server.fig \
 conclusion-1.fig conclusion-2.fig conclusion-3.fig

.SUFFIXES=.eps .fig

%.eps: %.fig
	fig2dev -L pstex $< $@

%.ps: %.fig
	fig2dev -L ps -P -z a4 -c $< $@


all: arch.ps slides

slides: exemple.ps  $(SLIDES:.fig=.ps)

exemple.ps: exemple.txt
	a2ps -2 -l 68 -L 40 -o $@ $<

arch.dvi: arch.tex server.eps requests.eps
	latex arch.tex

arch.ps: arch.dvi
	dvips -o $@ $<

clean:
	$(RM) *.toc *.ps *.dvi *.aux *.log *.eps *.bak *~

