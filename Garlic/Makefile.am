AUTOMAKE_OPTIONS = no-dependencies foreign

ADA_FOR_TARGET = @ADA_FOR_TARGET@

CONSTANTS_SH = $(srcdir)/constants.sh
CP           = cp
GARLIC       = $(prefix)/lib/garlic
GNATFLAGS    = -gnatg @GNATFLAGS@
INCFLAGS     = -I. -I$(srcdir)

GNATMAKE     = gnatmake
GNATBIND     = gnatbind

PLATFORM     = @PLATFORM_SPECIFIC@

TCPOBJECTS = \
s-gptcse.o\
s-gaprtc.o

TCPSOURCES = \
$(srcdir)/s-gaprtc.ads $(srcdir)/s-gaprtc.adb \
$(srcdir)/s-gptcse.ads $(srcdir)/s-gptcse.adb

Makefile: Makefile.fil

COMMONOBJECTS = \
s-galiop.o\
s-ganata.o\
s-ganeut.o\
s-gaplsp.o\
s-gaphlo.o\
s-gaprco.o\
s-gardeb.o\
s-garela.o\
s-garexc.o\
s-garfil.o\
s-gargro.o\
s-garhea.o\
s-garlic.o\
s-gaprre.o\
s-garopt.o\
s-garpar.o\
s-garpri.o\
s-gaprma.o\
s-garpro.o\
s-garrem.o\
s-garsta.o\
s-garstr.o\
s-gartab.o\
s-gartas.o\
s-gartra.o\
s-garter.o\
s-gartyp.o\
s-garuni.o\
s-garuti.o\
s-garsto.o\
s-gastco.o\
s-gastdf.o\
s-gasoli.o\
s-gastds.o\
s-ganota.o\
s-gprese.o\
s-parint.o\
s-rpc.o\
s-rpcser.o\
s-rpstio.o\
s-shasto.o\
s-stratt.o

COMMONSOURCES = \
          s-galiop.ads                        \
$(srcdir)/s-ganata.ads $(srcdir)/s-ganata.adb \
$(srcdir)/s-ganeut.ads $(srcdir)/s-ganeut.adb \
$(srcdir)/s-ganota.ads $(srcdir)/s-ganota.adb \
$(srcdir)/s-gaphlo.ads $(srcdir)/s-gaphlo.adb \
          s-gaplsp.ads                        \
$(srcdir)/s-gaprco.ads           s-gaprco.adb \
$(srcdir)/s-gaprre.ads $(srcdir)/s-gaprre.adb \
$(srcdir)/s-gardeb.ads $(srcdir)/s-gardeb.adb \
$(srcdir)/s-garexc.ads $(srcdir)/s-garexc.adb \
$(srcdir)/s-garfil.ads $(srcdir)/s-garfil.adb \
$(srcdir)/s-gargro.ads $(srcdir)/s-gargro.adb \
$(srcdir)/s-garhea.ads $(srcdir)/s-garhea.adb \
$(srcdir)/s-garlic.ads                        \
$(srcdir)/s-garopt.ads $(srcdir)/s-garopt.adb \
$(srcdir)/s-garpar.ads $(srcdir)/s-garpar.adb \
$(srcdir)/s-garpri.ads $(srcdir)/s-garpri.adb \
$(srcdir)/s-gaprma.ads $(srcdir)/s-gaprma.adb \
$(srcdir)/s-garpro.ads $(srcdir)/s-garpro.adb \
$(srcdir)/s-garrem.ads           s-garrem.adb \
$(srcdir)/s-garsta.ads $(srcdir)/s-garsta.adb \
$(srcdir)/s-garstr.ads $(srcdir)/s-garstr.adb \
$(srcdir)/s-gartas.ads $(srcdir)/s-gartas.adb \
$(srcdir)/s-gartra.ads $(srcdir)/s-gartra.adb \
$(srcdir)/s-garela.ads $(srcdir)/s-garela.adb \
$(srcdir)/s-garsto.ads $(srcdir)/s-garsto.adb \
$(srcdir)/s-gartab.ads $(srcdir)/s-gartab.adb \
$(srcdir)/s-garter.ads $(srcdir)/s-garter.adb \
$(srcdir)/s-gartyp.ads $(srcdir)/s-gartyp.adb \
$(srcdir)/s-garuni.ads $(srcdir)/s-garuni.adb \
$(srcdir)/s-garuti.ads $(srcdir)/s-garuti.adb \
$(srcdir)/s-gasoli.ads $(srcdir)/s-gasoli.adb \
$(srcdir)/s-gastco.ads $(srcdir)/s-gastco.adb \
$(srcdir)/s-gastdf.ads $(srcdir)/s-gastdf.adb \
$(srcdir)/s-gastds.ads $(srcdir)/s-gastds.adb \
$(srcdir)/s-gastha.ads $(srcdir)/s-gastha.adb \
$(srcdir)/s-gprese.ads                        \
$(srcdir)/s-parint.ads $(srcdir)/s-parint.adb \
$(srcdir)/s-rpc.ads    $(srcdir)/s-rpc.adb    \
$(srcdir)/s-rpcser.ads $(srcdir)/s-rpcser.adb \
$(srcdir)/s-rpstio.ads $(srcdir)/s-rpstio.adb \
$(srcdir)/s-shasto.ads $(srcdir)/s-shasto.adb \
$(srcdir)/s-stratt.adb

@FILTERSMAKEFILE@

OBJECTS = \
$(PROTOCOLSOBJECTS) \
$(FILTERSOBJECTS) \
$(COMMONOBJECTS)

SOURCES = \
$(PROTOCOLSSOURCES) \
$(FILTERSSOURCES) \
$(COMMONSOURCES)

CLEANFILES = lib*.a *.ali *.o split b_*.c b~*.ad[bs]\
	s-gaprco.adb s-galiop.ads\
	inlining gnatlib/* constants_nodef\
	b_gaprco$(EXEECT) isthrbio$(EXEEXT)\
	$(FILTERSOBJECTS)

DISTCLEANFILES = Makefile.fil s-garrem.adb

SUFFIXES = .adb .ads .ali .o .xrb

.adb.o:
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<

.ads.o:
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<

.adb.ali:
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<

.ads.ali:
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<

b_gaprco$(EXEEXT): $(srcdir)/b_gaprco.adb
	@${GNATMAKE} b_gaprco -o b_gaprco$(EXEEXT)

s-gaprco.adb: b_gaprco$(EXEEXT)
	@./b_gaprco$(EXEEXT) $(DEFPROTOCOLNAME) >s-gaprco.adb

s-garrem.adb:
	@if test -f $(srcdir)/5${PLATFORM_SPECIFIC}garrem.adb; then\
	  cp $(srcdir)/5${PLATFORM_SPECIFIC}garrem.adb s-garrem.adb;\
	else\
	  cp $(srcdir)/5egarrem.adb s-garrem.adb;\
	fi

force:

s-galiop.ads:
	@echo "private package System.Garlic.Linker_Options is" >s-galiop.ads
	@echo "" >>s-galiop.ads
	@if [ "x$(EXTRA_LIBS)" != "x" ] ; then\
	 for option in `echo $(EXTRA_LIBS)`; do\
	  echo "   pragma Linker_Options (\"$$option\");" >>s-galiop.ads;\
	 done;\
	fi
	@echo "" >>s-galiop.ads
	@echo "end System.Garlic.Linker_Options;" >>s-galiop.ads

used:
	grep '^D ' `echo $(OBJECTS) | $(SED) -e 's/\.o/.ali/g'` |\
		$(SED) 's/^.*://g' | sort |\
		$(AWK) -f unitused.awk | sort -n >deps
	grep '^W ' `echo $(OBJECTS) | $(SED) -e 's/\.o/.ali/g'` |\
		$(SED) 's/^.*://g' | sort |\
		$(AWK) -f unitused.awk | sort -n >withs


split: $(srcdir)/split.adb
	$(GNATMAKE) -m -o split $(srcdir)/split.adb

all-local: $(SOURCES) $(OBJECTS) $(LINKTEST) $(FILTERSINSTALL)

install-dirs-local:
	-rm -rf $(GARLIC)
	-$(mkinstalldirs) $(GARLIC)
	@echo $(GARLIC) created

inlining: all-local
	@-if test ! -f inlining; then\
	   rm -rf gnatlib >/dev/null 2>&1;\
	   mkdir gnatlib >/dev/null 2>&1;\
	   gnatlibdir=`$(srcdir)/../Dist/config.sdefault pathtolib`;\
	   for o in `cd $$gnatlibdir && echo *.ali`; do\
	     grep "D s-stratt.adb" $$gnatlibdir/$$o /dev/null;\
	   done | $(SED) 's,^.*[/]\(.*\)\.ali:.*$$,\1.o,g' > gnatlib/tocheck;\
	   for i in $(OBJECTS); do\
	      cat gnatlib/tocheck > gnatlib/tocheck.tmp;\
	      grep -v $$i gnatlib/tocheck.tmp > gnatlib/tocheck;\
	   done;\
	   if test -s gnatlib/tocheck; then\
	      echo go > inlining;\
	   else\
	      echo no > inlining;\
	   fi;\
	fi
	@-if test `cat inlining` = "go"; then\
	   for o in `cat gnatlib/tocheck`; do\
	      if [ ! -f gnatlib/$$o ]; then\
	          s=`gnatls -a -s $$o | sed 's/\013//g'`;\
	          echo\
                    "$(ADA_FOR_TARGET) -c $(CFLAGS) $(GNATFLAGS) -I. -I- $$s"\
	            "-o gnatlib/$$o";\
	          $(ADA_FOR_TARGET) -c $(CFLAGS) $(GNATFLAGS) -I. -I-\
                    $$s -o gnatlib/$$o;\
	          if test $$? = 0; then\
	             echo $$o >>gnatlib/rebuild;\
	          else\
	             $(RM) -f gnatlib/tocheck;\
	             break;\
	          fi;\
	      fi;\
	   done;\
	   echo yes > inlining;\
	fi

linktest: $(srcdir)/linktest.adb $(OBJECTS)
	$(GNATMAKE) -c -a -m -I$(srcdir) -I. $(srcdir)/linktest.adb
	$(GNATBIND) $(srcdir)/linktest.ali -I. -I$(srcdir)

install-extra-libs: $(FILTERSEXTRALIBS) inlining install-dirs-local

install-data-local: all-local install-dirs-local install-extra-libs 
	@if test ! -f inlining; then\
	   exit 1;\
	fi
	@-for lib in libgnat.a libgnarl.a libAtasking.a libPtasking.a; do \
	  pathtolib=`$(srcdir)/../Dist/config.sdefault pathtolib`;\
	  if test -f $$pathtolib$$lib; then \
	    $(CP)  $$pathtolib$$lib $(GARLIC)/$$lib;\
	    chmod u+w $(GARLIC)/$$lib;\
	    $(AR) d $(GARLIC)/$$lib s-stratt.o \
	                            s-rpc.o \
	                            s-shasto.o \
	                            s-parint.o 2>/dev/null;\
	    if test `cat inlining` = "yes"; then\
	      (cd gnatlib;\
	       for o in `cat rebuild`; do \
		 $(AR) d $(GARLIC)/$$lib $$o 2>/dev/null;\
		 if test $$? = 0; then \
		   $(AR) cq $(GARLIC)/$$lib $$o;\
	           echo Replace inlined $$o in $$lib;\
	         fi;\
	       done);\
	    fi;\
	    $(RANLIB) $(GARLIC)/$$lib;\
	  fi;\
	done
	@if test `cat inlining` = "yes"; then\
	  (cd gnatlib;\
	   $(CP) $(CP_PRESERVE) `$(SED) 's/\.o/.ali/g' rebuild` $(GARLIC));\
	fi
	for i in $(SOURCES)\
		`echo $(OBJECTS) | $(SED) -e 's/\.o/.ali/g'`; do\
		if test -f $$i; then\
			$(CP) $(CP_PRESERVE) $$i $(GARLIC)/;\
		fi;\
	done
	$(AR) cq $(GARLIC)/libgarlic.a $(OBJECTS)
	$(RANLIB) $(GARLIC)/libgarlic.a
	chmod a+r $(GARLIC)/*
	chmod a-w $(GARLIC)/*

# Dependencies

depend: $(SOURCES)
	$(MAKE)
	$(srcdir)/mkdep
	$(MAKE) Makefile

###
### The following section is valid only for developpers that use the
### --enable-debug flag on configure. If you want to make modifications
### outside the development mode, you need to either update the following
### dependencies or to use the "fast" target.
###
### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
b_gaprco.o: b_gaprco.adb
s-gafizi.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garfil.ads s-gafizi.ads s-gafizi.adb s-garhea.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads
s-galiop.o: s-garlic.ads s-galiop.ads
s-ganata.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-ganata.ads s-ganata.adb s-gasoli.ads s-gasoli.adb s-gartab.ads s-gartab.adb s-gartyp.ads s-garuti.ads s-stratt.adb
s-ganeut.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-ganeut.ads s-ganeut.adb s-gasoli.ads s-gartyp.ads
s-ganota.o: s-garlic.ads s-garexc.ads s-ganota.ads s-ganota.adb s-garpro.ads s-gasoli.ads s-gartyp.ads
s-gaphlo.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-gaphlo.ads s-gaphlo.adb s-garpro.ads s-gasoli.ads s-gartyp.ads s-garuti.ads
s-gaplsp.o: s-garlic.ads s-gaplsp.ads
s-gaprco.o: s-garlic.ads s-garexc.ads s-garpro.ads s-gaprco.ads s-gaprco.adb s-gaprtc.ads s-gptcse.ads s-gartyp.ads
s-gaprma.o: s-garlic.ads s-garpri.ads s-gaprma.ads s-gaprma.adb
s-gaprre.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garhea.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-gaprre.ads s-gaprre.adb s-gasoli.ads s-gastha.ads s-garstr.ads s-gartra.ads s-gartyp.ads
s-gaprtc.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garfil.ads s-garhea.ads s-garhea.adb s-galiop.ads s-ganata.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-gaplsp.ads s-garpro.ads s-gaprtc.ads s-gaprtc.adb s-gasoli.ads s-gasoli.adb s-gastha.ads s-garsto.ads s-garstr.ads s-gartab.ads s-gartab.adb s-gartra.ads s-gartyp.ads s-garuti.ads
s-gardeb.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-gasoli.ads s-gartyp.ads
s-garela.o: s-garlic.ads s-garela.ads s-garela.adb s-garopt.ads s-gasoli.ads s-gartas.ads s-gartyp.ads
s-garexc.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garexc.adb s-gasoli.ads s-gartyp.ads
s-garfil.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garfil.ads s-garfil.adb s-garhea.ads s-ganata.ads s-ganata.adb s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-gastha.ads s-garstr.ads s-garstr.adb s-gartab.ads s-gartab.adb s-gartyp.ads s-garuti.ads s-stratt.adb
s-gargro.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-gargro.ads s-gargro.adb s-garhea.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-gastha.ads s-garstr.ads s-garstr.adb s-gartyp.ads s-stratt.adb
s-garhea.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garfil.ads s-gargro.ads s-garhea.ads s-garhea.adb s-galiop.ads s-garopt.ads s-garpar.ads s-garpar.adb s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-gastha.ads s-garsto.ads s-garstr.ads s-garstr.adb s-gartab.ads s-gartra.ads s-gartyp.ads s-garuti.ads
s-garlic.o: s-garlic.ads
s-garopt.o: s-garlic.ads s-garopt.ads s-garopt.adb s-gaplsp.ads s-gartyp.ads s-garuti.ads
s-garpar.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-gargro.ads s-garhea.ads s-ganata.ads s-garopt.ads s-garpar.ads s-garpar.adb s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-gastha.ads s-garstr.ads s-gartab.ads s-gartab.adb s-gartyp.ads s-garuti.ads s-stratt.adb
s-garpri.o: s-garlic.ads s-garpri.ads s-garpri.adb
s-garpro.o: s-garlic.ads s-garexc.ads s-garpro.ads s-garpro.adb s-gartyp.ads
s-garrem.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-garrem.ads s-garrem.adb s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garsta.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garela.ads s-garexc.ads s-garexc.adb s-garfil.ads s-gargro.ads s-garhea.ads s-ganata.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gaprco.ads s-garrem.ads s-gasoli.ads s-garsta.ads s-garsta.adb s-gastha.ads s-garsto.ads s-gastco.ads s-garstr.ads s-garter.ads s-gartra.ads s-gartyp.ads s-garuni.ads s-garuti.ads
s-garsto.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garpar.ads s-gaphlo.ads s-gaplsp.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garsto.ads s-garsto.adb s-garstr.ads s-gartyp.ads s-garuni.ads
s-garstr.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-gasoli.ads s-gasoli.adb s-gastha.ads s-gastha.adb s-garstr.ads s-garstr.adb s-gartyp.ads s-stratt.adb
s-gartab.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-ganata.ads s-gasoli.ads s-gartab.ads s-gartab.adb s-gartyp.ads
s-gartas.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-gasoli.ads s-gartas.ads s-gartas.adb s-gartyp.ads s-stratt.adb
s-garter.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garela.ads s-garexc.ads s-garfil.ads s-garhea.ads s-garhea.adb s-galiop.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-gastha.ads s-garsto.ads s-garstr.ads s-garstr.adb s-garter.ads s-garter.adb s-gartra.ads s-gartyp.ads s-garuti.ads s-stratt.adb
s-gartra.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garopt.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartra.ads s-gartra.adb s-gartyp.ads s-stratt.adb
s-gartyp.o: s-garlic.ads s-gartyp.ads s-gartyp.adb
s-garuni.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garfil.ads s-gargro.ads s-garhea.ads s-garhea.adb s-galiop.ads s-ganata.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-gastha.ads s-garsto.ads s-garstr.ads s-garstr.adb s-gartab.ads s-gartab.adb s-gartra.ads s-gartyp.ads s-garuni.ads s-garuni.adb s-garuti.ads s-stratt.adb
s-garuti.o: s-garlic.ads s-garuti.ads s-garuti.adb
s-gasoli.o: s-garlic.ads s-gasoli.ads s-gasoli.adb s-gartyp.ads s-stratt.adb
s-gastco.o: s-garlic.ads s-garexc.ads s-garsto.ads s-gastco.ads s-gastco.adb s-gartyp.ads
s-gastdf.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-garsto.ads s-gastdf.ads s-gastdf.adb s-gartyp.ads
s-gastds.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garexc.adb s-garhea.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-gastha.ads s-garsto.ads s-gastds.ads s-gastds.adb s-garstr.ads s-gartyp.ads s-garuni.ads s-stratt.adb
s-gprese.o: s-garlic.ads s-garexc.ads s-garpro.ads s-gaprre.ads s-gprese.ads s-gartyp.ads
s-gptcse.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garpro.ads s-gaprtc.ads s-gptcse.ads s-gptcse.adb s-gasoli.ads s-gartyp.ads
s-parint.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garhea.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-garsta.ads s-gastha.ads s-garsto.ads s-garstr.ads s-gartyp.ads s-garuni.ads s-garuti.ads s-garuti.adb s-parint.ads s-parint.adb s-rpc.ads s-stratt.adb
s-rpc.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garfil.ads s-garhea.ads s-garhea.adb s-galiop.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpri.ads s-gaprma.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-garsta.ads s-gastha.ads s-garsto.ads s-garstr.ads s-garstr.adb s-gartra.ads s-gartyp.ads s-garuni.ads s-garuti.ads s-rpc.ads s-rpc.adb s-stratt.adb
s-rpcser.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garhea.ads s-garopt.ads s-garpri.ads s-gaprma.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-garsta.ads s-gastha.ads s-garstr.ads s-garstr.adb s-gartas.ads s-gartyp.ads s-rpc.ads s-rpcser.ads s-rpcser.adb s-stratt.adb
s-rpstio.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garexc.ads s-garhea.ads s-ganata.ads s-garpro.ads s-gasoli.ads s-gasoli.adb s-garsta.ads s-gastha.ads s-garstr.ads s-garstr.adb s-gartab.ads s-gartab.adb s-gartyp.ads s-rpc.ads s-rpstio.ads s-rpstio.adb
s-shasto.o: s-garlic.ads s-gardeb.ads s-garexc.ads s-garexc.adb s-garsto.ads s-gartyp.ads s-shasto.ads s-shasto.adb
s-stratt.o: s-stratt.adb
