CP = /bin/cp
SED = /bin/sed

CFGFILES=\
myconfig.cfg

IDLFILES=\
naming.idl

FIGFILES=\
xe-arch.fig\
full-ex.fig\
corba-arch.fig

ADAFILES=\
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

SEDFILES=\
ada.sed\
cfg.sed\
idl.sed

TEXIFILES = $(ADAFILES:=.texi) $(CFGFILES:=.texi) $(IDLFILES:=.texi)

JPEGFILES = $(FIGFILES:=.jpeg)
PDFFILES  = $(FIGFILES:=.pdf)
EPSFILES  = $(FIGFILES:=.eps)

GUIDE=glade_ug

GRAPHS=$(CONFIGS) $(IMAGES)

default: $(GUIDE).ps

all: $(GUIDE).ps $(GUIDE).info $(GUIDE).html $(GUIDE).pdf

$(GUIDE).html: $(GUIDE).texi $(JPEGFILES) $(SEDFILES)
	texi2html -menu -monolithic $(GUIDE).texi

$(GUIDE).info: $(GUIDE).texi $(SEDFILES)
	makeinfo $(GUIDE).texi

$(GUIDE).ps: $(GUIDE).dvi $(SEDFILES)
	dvips -o $(GUIDE).ps $(GUIDE).dvi

$(GUIDE).dvi: $(GUIDE).texi $(TEXIFILES) $(EPSFILES) $(SEDFILES)
	texi2dvi $(GUIDE).texi

$(GUIDE).pdf: $(GUIDE).texi $(PDFFILES) $(SEDFILES)
	texi2pdf $(GUIDE).texi

$(ADAFILES): all-srcs.ada
	gnatchop -w all-srcs.ada

%.cfg.texi: %.cfg cfg.sed
	gentexifile $< $@

%.idl.texi: %.idl idl.sed
	gentexifile $< $@

%.ads.texi: %.ads ada.sed
	gentexifile $< $@

%.adb.texi: %.adb ada.sed
	gentexifile $< $@

%.fig.eps: %.fig
	fig2dev -L ps $< $@

%.pdf: %.pdf
	convert $< $@

%.jpeg:	%.eps
	convert $< $@

%.sed: %.kw
	gensedfile $< $@

clean:
	rm -f *.aux *.log *.ads *.adb *.dvi *.ps *.eps *.html *.pdf
	rm -f *.jpeg *.cp *.info *.pg *.toc *.vr *.fn *.ky *.tp *~
	rm -f *.*.texi
