CP = /bin/cp
SED = sed
A2PSFLAGS = -1 -B --border=no --font-size=9

CONFIGS=\
myconfig.cfg\
naming.idl

IMAGES=\
xe-arch.fig\
full-ex.fig\
corba-arch.fig

SOURCES=\
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
normalinstantiation.ads

GUIDE=glade_ug

GRAPHS=$(SOURCES) $(CONFIGS) $(IMAGES)

default:
	$(MAKE) $(GUIDE).ps $(GUIDE).info

all:
	$(MAKE) $(GUIDE).ps $(GUIDE).info $(GUIDE).html $(GUIDE).pdf

ps: $(GUIDE).ps

info: $(GUIDE).info

html: $(GUIDE).html

pdf: $(GUIDE).pdf

txtfiles: ${SOURCES} ${CONFIGS}
	make `echo $(SOURCES) $(CONFIGS) | sed 's,\([^ ][^ ]*\),\1.txt,g'`
	touch txtfiles

jpegfiles: epsfiles
	make `echo $(GRAPHS) | sed 's,\([^ ][^ ]*\),\1.jpeg,g'`
	touch jpegfiles

epsfiles: ${GRAPHS}
	make `echo $(GRAPHS) | sed 's,\([^ ][^ ]*\),\1.eps,g'`
	touch epsfiles

pdffiles: epsfiles
	make `echo $(GRAPHS) | sed 's,\([^ ][^ ]*\),\1.pdf,g'`
	touch pdffiles

allfiles: txtfiles epsfiles jpegfiles

$(GUIDE).html: $(GUIDE).texi jpegfiles
	makeinfo --html $(GUIDE).texi

$(GUIDE).info: $(GUIDE).texi txtfiles
	makeinfo $<

$(GUIDE).ps: $(GUIDE).dvi
	dvips -o $(GUIDE).ps $(GUIDE).dvi

$(GUIDE).dvi: $(GUIDE).texi epsfiles txtfiles
	texi2dvi $(GUIDE).texi

$(GUIDE).pdf: $(GUIDE).texi pdffiles
	texi2pdf $(GUIDE).texi

${SOURCES}: all-srcs.ada
	gnatchop -w $<

%.pdf: %.eps
	epstopdf $<

%.ads.txt: %.ads
	${CP} -f $< $@

%.adb.txt: %.adb
	${CP} -f $< $@

%.cfg.txt: %.cfg
	${CP} -f $< $@

%.idl.txt: %.idl
	${CP} -f $< $@

%.cfg.eps: %.cfg
	a2ps $(A2PSFLAGS) -Eada --line-numbers=1 $< -o $<.tmp.ps
	ps2epsi $<.tmp.ps $@
	./adjustbb $@
	${RM} $<.tmp.ps

%.ads.eps: %.ads
	a2ps $(A2PSFLAGS) -Eada $< -o $<.tmp.ps
	ps2epsi $<.tmp.ps $@
	./adjustbb $@
	${RM} $<.tmp.ps

%.adb.eps: %.adb
	a2ps $(A2PSFLAGS) -Eada $< -o $<.tmp.ps
	ps2epsi $<.tmp.ps $@
	./adjustbb $@
	${RM} $<.tmp.ps

%.idl.eps: %.idl
	a2ps $(A2PSFLAGS) -Ecorba.ssh $< -o $<.tmp.ps
	ps2epsi $<.tmp.ps $@
	./adjustbb $@
	${RM} $<.tmp.ps

%.ads.jpeg: %.ads.eps
	convert $< $@

%.adb.jpeg: %.adb.eps
	convert $< $@

%.jpeg:	%.eps
	convert $< $@

%.fig.eps: %.fig
	fig2dev -L ps $< $@

clean:
	rm -f *.aux *.log *.ads *.adb *.dvi *.ps *.eps *.ad?.txt *.html *.pdf
	rm -f *.jpeg *.cp *.info *.pg *.toc *.vr *.fn *.ky *.tp *~ *.idl.txt
	rm -f txtfiles epsfiles jpegfiles pdffiles
