CP = /bin/cp
SED = /bin/sed
SHELL=/bin/sh
FIG2DEV=fig2dev

CFGFILES=\
myconfig.cfg

IDLFILES=\
naming.idl

FIGFILES=\
xe-arch.fig\
full-ex.fig\
corba-arch.fig

ADAFILES=\
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

SEDFILES=\
ada.sed\
cfg.sed\
idl.sed

TEXIFILES = $(ADAFILES:=.texi) $(CFGFILES:=.texi) $(IDLFILES:=.texi) gfdl.texi

JPGFILES = $(FIGFILES:=.jpg)
EPSFILES = $(FIGFILES:=.eps)
TXTFILES = $(FIGFILES:=.txt)

GUIDE=glade_ug

GRAPHS=$(CONFIGS) $(IMAGES)

default: $(GUIDE).ps

all: $(GUIDE).ps $(GUIDE).info $(GUIDE).html $(GUIDE).txt

$(GUIDE).texi:  $(TEXIFILES)

$(GUIDE).html: $(GUIDE).texi $(JPGFILES) $(SEDFILES)
	makeinfo --html --no-split $(GUIDE).texi

$(GUIDE).info: $(GUIDE).texi $(SEDFILES)
	makeinfo $(GUIDE).texi

$(GUIDE).ps: $(GUIDE).dvi $(SEDFILES)
	dvips -o $(GUIDE).ps $(GUIDE).dvi

$(GUIDE).dvi: $(GUIDE).texi $(TEXIFILES) $(EPSFILES) $(SEDFILES)
	texi2dvi $(GUIDE).texi

$(GUIDE).pdf: $(GUIDE).texi $(PDFFILES) $(SEDFILES)
	texi2pdf $(GUIDE).texi

$(GUIDE).txt: $(GUIDE).texi $(TXTFILES) $(SEDFILES)
	makeinfo --force --no-headers --no-split -o $(GUIDE).txt $(GUIDE).texi

$(ADAFILES): all-srcs.ada
	gnatchop -w all-srcs.ada

%.cfg.texi: %.cfg cfg.sed
	./gentexifile $<

%.idl.texi: %.idl idl.sed
	./gentexifile $<

%.ads.texi: %.ads ada.sed
	./gentexifile $<

%.adb.texi: %.adb ada.sed
	./gentexifile $<

%.fig.eps: %.fig
	$(FIG2DEV) -L ps $< $@

%.fig.jpg:	%.fig
	$(FIG2DEV) -L jpeg $< $@

%.sed: %.kw
	./gensedfile $< $@

myconfig.cfg.texi: myconfig.cfg cfg.sed
	@awk '{printf "@b{%02d} %s\n", NR, $$0}' <myconfig.cfg >myconfig.tmp.cfg
	./gentexifile myconfig.tmp.cfg $@
	@$(RM) myconfig.tmp.cfg

clean:
	@rm -f *.aux *.log *.ads *.adb *.dvi *.eps  *.tmp
	@rm -f *.cp *.pg *.toc *.vr *.fn *.ky *.tp *~ *.*.texi

veryclean: clean
	@rm -f $(GUIDE).info* $(GUIDE).txt $(GUIDE).html *.jpg $(GUIDE).ps
	@rm -f ada.sed idl.sed cfg.sed
	@rm -fr $(GUIDE)

release: all
	@rm -f MANIFEST
	@echo ada.kw idl.kw cfg.kw       >>MANIFEST
	@echo all-srcs.ada               >>MANIFEST
	@echo gentexifile gensedfile     >>MANIFEST
	@echo texinfo.tex  Makefile      >>MANIFEST
	@echo $(TXTFILES)                >>MANIFEST
	@echo $(FIGFILES)                >>MANIFEST
	@echo $(JPGFILES)                >>MANIFEST
	@echo $(CFGFILES)                >>MANIFEST
	@echo $(IDLFILES)                >>MANIFEST
	@echo gfdl.texi                  >>MANIFEST
	@echo $(GUIDE).info*             >>MANIFEST
	@echo $(GUIDE).html              >>MANIFEST
	@echo $(GUIDE).ps                >>MANIFEST
	@echo $(GUIDE).txt               >>MANIFEST
	@echo $(GUIDE).texi              >>MANIFEST
