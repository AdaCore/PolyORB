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
$(srcdir)/s-garpri.ads                        \
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

DISTCLEANFILES = config.h Makefile.fil

SUFFIXES = .adb .ads .ali .o .xrb

.adb.o:
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<

.ads.o:
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<

.adb.ali:
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<

.ads.ali:
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<

constants_nodef: $(srcdir)/constants.c config.h
	$(CC) -o constants_nodef -I. -UCONSTANT_NAME $(srcdir)/constants.c

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

platformfile: $(CONSTANTS_SH) split constants_nodef config.h
	@echo Generating $(srcdir)/5$(PLATFORM_SPECIFIC)gasoco.ads
	$(CONSTANTS_SH) $(srcdir)/5$(PLATFORM_SPECIFIC)gasoco.ads \
		$(srcdir) "$(PSNAME)" gasoco.txt
	@echo $(srcdir)/5$(PLATFORM_SPECIFIC)gasoco.ads generated

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
