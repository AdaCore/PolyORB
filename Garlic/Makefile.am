#
## Makefile.am,v 1.11 1996/05/19 14:03:21 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

ADA_FOR_TARGET = @ADA_FOR_TARGET@

CONSTANTS_SH = $(srcdir)/constants.sh
CP           = cp
GARLIC       = $(prefix)/lib/garlic
GNATFLAGS    = -gnatg @GNATFLAGS@
INCFLAGS     = -I. -I$(srcdir)

GNATMAKE     = gnatmake
GNATBIND     = gnatbind

PLATFORM            = @PLATFORM_SPECIFIC@

CONSTANTS_PLATFORM  = $(srcdir)/5$(PLATFORM)garcon.ads

LAUNCHINGOBJECTS = \
s-gaplsp.o\
s-garnam.o\
s-garrem.o

LAUNCHINGSOURCES = \
          s-gaplsp.ads                        \
$(srcdir)/s-garnam.ads $(srcdir)/s-garnam.adb \
$(srcdir)/s-garrem.ads $(srcdir)/s-garrem.adb

TCPOBJECTS = \
s-gtplsp.o\
s-gartcp.o

TCPSOURCES = \
$(TCP_PLATFORM_SOURCES)                       \
$(srcdir)/s-gartcp.ads $(srcdir)/s-gartcp.adb

NOBLTCPOBJECTS = \
s-ganobl.o

NOBLTCPSOURCES = \
$(srcdir)/s-ganobl.ads $(srcdir)/s-ganobl.adb

Makefile: Makefile.fil

@FILTERFILE@

COMMONOBJECTS = \
s-galiop.o\
s-galite.o\
s-ganata.o\
s-ganeut.o\
s-gaphlo.o\
s-gaprob.o\
s-garcon.o\
s-gardeb.o\
s-garexc.o\
s-garfil.o\
s-gargro.o\
s-garhea.o\
s-garlic.o\
s-garopt.o\
s-garela.o\
s-garpar.o\
s-garpri.o\
s-garpro.o\
s-garrep.o\
s-garser.o\
s-garsta.o\
s-gartra.o\
s-gaprco.o\
s-garstr.o\
s-gartab.o\
s-garthi.o\
s-garter.o\
s-gartyp.o\
s-garuni.o\
s-garuti.o\
s-gasoli.o\
s-gatcop.o\
s-parint.o\
s-rpc.o\
s-rpcpoo.o\
s-rpstio.o\
s-stratt.o

COMMONSOURCES = \
          s-galiop.ads                        \
$(srcdir)/s-galite.ads $(srcdir)/s-galite.adb \
$(srcdir)/s-ganeut.ads $(srcdir)/s-ganeut.adb \
$(srcdir)/s-gaphlo.ads $(srcdir)/s-gaphlo.adb \
$(srcdir)/s-gaprob.ads $(srcdir)/s-gaprob.adb \
          s-garcon.ads                        \
$(srcdir)/s-gardeb.ads $(srcdir)/s-gardeb.adb \
$(srcdir)/s-garfil.ads $(srcdir)/s-garfil.adb \
$(srcdir)/s-gargro.ads $(srcdir)/s-gargro.adb \
$(srcdir)/s-garhea.ads $(srcdir)/s-garhea.adb \
$(srcdir)/s-garlic.ads                        \
$(srcdir)/s-garopt.ads $(srcdir)/s-garopt.adb \
$(srcdir)/s-garpar.ads $(srcdir)/s-garpar.adb \
$(srcdir)/s-garpro.ads                        \
$(srcdir)/s-garpri.ads                        \
$(srcdir)/s-ganata.ads $(srcdir)/s-ganata.adb \
$(srcdir)/s-garpro.ads $(srcdir)/s-garpro.adb \
$(srcdir)/s-garexc.ads $(srcdir)/s-garexc.adb \
$(srcdir)/s-garrep.ads $(srcdir)/s-garrep.adb \
$(srcdir)/s-garser.ads           s-garser.adb \
$(srcdir)/s-garsta.ads $(srcdir)/s-garsta.adb \
$(srcdir)/s-garstr.ads $(srcdir)/s-garstr.adb \
$(srcdir)/s-garthi.ads                        \
$(srcdir)/s-gartra.ads $(srcdir)/s-gartra.adb \
$(srcdir)/s-gaprco.ads           s-gaprco.adb \
$(srcdir)/s-garela.ads $(srcdir)/s-garela.adb \
$(srcdir)/s-garter.ads $(srcdir)/s-garter.adb \
$(srcdir)/s-gartab.ads $(srcdir)/s-gartab.adb \
$(srcdir)/s-gartyp.ads                        \
$(srcdir)/s-garuni.ads $(srcdir)/s-garuni.adb \
$(srcdir)/s-garuti.ads $(srcdir)/s-garuti.adb \
$(srcdir)/s-gasoli.ads $(srcdir)/s-gasoli.adb \
          s-gatcop.ads                        \
$(srcdir)/s-gastha.ads $(srcdir)/s-gastha.adb \
$(srcdir)/s-parint.ads $(srcdir)/s-parint.adb \
$(srcdir)/s-rpc.ads    $(srcdir)/s-rpc.adb    \
$(srcdir)/s-rpcpoo.ads $(srcdir)/s-rpcpoo.adb \
$(srcdir)/s-rpstio.ads $(srcdir)/s-rpstio.adb \
$(srcdir)/s-stratt.adb

OBJECTS = \
@PROTOCOLSOBJECTS@ \
@LAUNCHINGOBJECTS@ \
$(FILTERSOBJECTS) \
$(COMMONOBJECTS)

SOURCES = \
@PROTOCOLSSOURCES@ \
@LAUNCHINGSOURCES@ \
$(FILTERSSOURCES) \
$(COMMONSOURCES)

CLEANFILES = *.ali s-garcon.ads *.o s-gaprco.adb s-galiop.ads\
	s-garser.adb s-gaplsp.ads $(FILTERSOBJECTS) split\
	inlining gnatlib/* constants_nodef\
	b_*.c isthrbio$(EXEEXT)

DISTCLEANFILES = config.h $(TCP_PLATFORM_SOURCES) s-gatcop.ads \
	Makefile.fil

SUFFIXES = .adb .ads .ali .o .xrb

isthrbio$(EXEEXT): $(srcdir)/isthrbio.adb Makefile $(TCP_PLATFORM_SOURCES)
	$(GNATMAKE) -a $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) -I$(srcdir) -I. \
		$(srcdir)/isthrbio.adb

s-gtplsp.o: $(TCP_PLATFORM_SOURCES)
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) \
		`echo $(TCP_PLATFORM_SOURCES) | $(SED) -e 's/ .*$$//'`

%.o: %.adb
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<

%.o: %.ads
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<


%.ali: %.adb
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<


%.ali: %.ads
	$(ADA_FOR_TARGET) -c $(CFLAGS) $(INCFLAGS) $(GNATFLAGS) $<

constants_nodef: $(srcdir)/constants.c config.h
	$(CC) -o constants_nodef -I. -UCONSTANT_NAME $(srcdir)/constants.c

s-garcon.ads: $(CONSTANTS_SH)
	if test -f $(CONSTANTS_PLATFORM); then\
	  cp $(CONSTANTS_PLATFORM) s-garcon.ads;\
	else\
	  $(MAKE) split constants_nodef config.h; \
	  $(SHELL) $(CONSTANTS_SH) System.Garlic.Constants $(srcdir);\
	fi

platformfile: $(CONSTANTS_SH) split constants_nodef config.h
	@echo Generating $(srcdir)/5$(PLATFORM_SPECIFIC)garcon.ads
	$(CONSTANTS_SH) $(srcdir)/5$(PLATFORM_SPECIFIC)garcon.ads \
		$(srcdir) "$(PSNAME)" garcon.txt
	@echo $(srcdir)/5$(PLATFORM_SPECIFIC)garcon.ads generated

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

# This one deserves some comment: the Makefile variable ISTHRBIO will
# be used to set the shell variable of the same name, whose value will
# in turn be used to create the s-gatcop.ads file. Modifying a shell
# variable implies that the whole rule stays in a single shell
# statement, hence the backslashed lines.

s-gatcop.ads: Makefile
	if test $(ISTHRBIO) = "test"; then \
	  $(MAKE) isthrbio$(EXEEXT); \
	  if ./isthrbio; then \
	    ISTHRBIO="no"; \
	  else \
	    ISTHRBIO="yes"; \
	  fi; \
        else \
	  ISTHRBIO=$(ISTHRBIO); \
	fi; \
	if test $$ISTHRBIO = "yes"; then \
	  S_GATCOP_SUBST="System.Garlic.Thin"; \
	else \
	  S_GATCOP_SUBST="System.Garlic.Non_Blocking"; \
	fi; \
	echo "with $$S_GATCOP_SUBST;" > s-gatcop.ads; \
	echo "package System.Garlic.TCP.Operations" >> s-gatcop.ads; \
	echo "  renames $$S_GATCOP_SUBST;" >> s-gatcop.ads

s-gatcop.o: $(srcdir)/s-garthi.ads $(srcdir)/s-ganobl.adb

$(TCP_PLATFORM_SOURCES): $(S_GTPLSP_ADS) $(S_GTPLSP_ADB)
	cp $(srcdir)/$(S_GTPLSP_ADS) s-gtplsp.ads
	@if test -n "$(S_GTPLSP_ADB)"; then \
	  cp $(srcdir)/$(S_GTPLSP_ADB) s-gtplsp.adb; \
	fi

split: $(srcdir)/split.adb
	$(GNATMAKE) -m -o split $(srcdir)/split.adb

all-local: $(SOURCES) $(OBJECTS) $(LINKTEST) $(FILTERSBUILD)

install-dirs-local:
	-rm -rf $(GARLIC)
	$(mkinstalldirs) $(GARLIC)

inlining: all-local
	@if test ! -f inlining; then\
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
	@if test `cat inlining` = "go"; then\
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
	             $(RM) gnatlib/tocheck;\
	             break;\
	          fi;\
	      fi;\
	   done;\
	   echo yes > inlining;\
	fi

linktest: $(srcdir)/linktest.adb $(OBJECTS)
	$(GNATMAKE) -c -a -m -I$(srcdir) -I. $(srcdir)/linktest.adb
	$(GNATBIND) $(srcdir)/linktest.ali -I. -I$(srcdir)
	@$(GNATBIND) -l $(srcdir)/linktest.ali -I. -I$(srcdir) \
	   | grep 'system.garlic.*body' | sed -n '1,1p' \
	   | $(SED) 's/^.*garlic.\(.*\) .*$$/\1/g' > first;
	@if test `cat first` != "debug"; then\
	   echo "system.garlic.debug should be elaborted first";\
	   exit 1;\
	fi

install-extra-libs: $(FILTERSINSTALL) inlining install-dirs-local

install-data-local: all-local install-dirs-local install-extra-libs 
	@if test ! -f inlining; then\
	   exit 1;\
	fi
	@for lib in libgnat.a libgnarl.a libAtasking.a libPtasking.a; do \
	  pathtolib=`$(srcdir)/../Dist/config.sdefault pathtolib`;\
	  if test -f $$pathtolib$$lib; then \
	    echo Replacing inlined subprograms in $$lib;\
	    $(CP)  $$pathtolib$$lib $(GARLIC)/$$lib;\
	    chmod u+w $(GARLIC)/$$lib;\
	    $(AR) d $(GARLIC)/$$lib s-stratt.o s-rpc.o s-parint.o;\
	    if test `cat inlining` = "yes"; then\
	      (cd gnatlib;\
	       $(AR) d $(GARLIC)/$$lib `cat rebuild`;\
	       $(AR) cq $(GARLIC)/$$lib `cat rebuild`);\
	    fi;\
	    $(RANLIB) $(GARLIC)/$$lib;\
	  fi;\
	done
	@if test `cat inlining` = "yes"; then\
	  (cd gnatlib;\
	   $(CP) $(CP_PRESERVE) `$(SED) 's/\.o/.ali/g' rebuild` $(GARLIC));\
	fi
	for i in $(GARLIC) $(SOURCES)\
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
b~isthrbio.o: b~isthrbio.ads b~isthrbio.adb
isthrbio.o: isthrbio.adb s-garlic.ads s-gtplsp.ads s-garthi.ads
linktest.o: linktest.adb s-garlic.ads s-gardeb.ads s-ganobl.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-garter.ads s-garthi.ads s-gartyp.ads s-garuti.ads s-parint.ads s-rpc.ads
s-gafizi.o: s-garlic.ads s-gardeb.ads s-garfil.ads s-gafizi.ads s-gafizi.adb s-garhea.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-galiop.o: s-garlic.ads s-galiop.ads
s-galite.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-galite.ads s-galite.adb s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-ganata.o: s-garlic.ads s-gardeb.ads s-ganata.ads s-ganata.adb s-gasoli.ads s-gartab.ads s-gartab.adb s-garuti.ads
s-ganeut.o: s-garlic.ads s-gardeb.ads s-ganeut.ads s-ganeut.adb s-garthi.ads
s-ganobl.o: s-garlic.ads s-garcon.ads s-gardeb.ads s-ganobl.ads s-ganobl.adb s-garpri.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartcp.ads s-garthi.ads s-gartyp.ads s-garuti.ads
s-gaphlo.o: s-garlic.ads s-gardeb.ads s-gaphlo.ads s-gaphlo.adb s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-gaplsp.o: s-garlic.ads s-gaplsp.ads
s-gaprco.o: s-garlic.ads s-gardeb.ads s-garpro.ads s-gaprco.ads s-gaprco.adb s-garrep.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartcp.ads s-gartyp.ads s-garuti.ads
s-gaprob.o: s-garlic.ads s-gardeb.ads s-gaprob.ads s-gaprob.adb s-gasoli.ads s-garuti.ads
s-garcon.o: s-garlic.ads s-garcon.ads
s-gardeb.o: s-garlic.ads s-gardeb.ads s-gardeb.adb
s-garela.o: s-garlic.ads s-gardeb.ads s-garela.ads s-garela.adb s-garopt.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garexc.o: s-garlic.ads s-gardeb.ads s-garexc.ads s-garexc.adb
s-garfil.o: s-garlic.ads s-gardeb.ads s-garfil.ads s-garfil.adb s-garhea.ads s-ganata.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartab.ads s-gartab.adb s-gartyp.ads s-garuti.ads
s-gargro.o: s-garlic.ads s-gardeb.ads s-gargro.ads s-gargro.adb s-garhea.ads s-ganata.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garhea.o: s-garlic.ads s-gardeb.ads s-garfil.ads s-garhea.ads s-garhea.adb s-galiop.ads s-ganata.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartra.ads s-gartyp.ads s-garuti.ads
s-garlic.o: s-garlic.ads
s-garnam.o: s-garlic.ads s-garcon.ads s-gardeb.ads s-garnam.ads s-garnam.adb s-gasoli.ads s-garthi.ads s-garuti.ads
s-garopt.o: s-garlic.ads s-gardeb.ads s-garopt.ads s-garopt.adb s-gaplsp.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garpar.o: s-garlic.ads s-gardeb.ads s-gargro.ads s-garhea.ads s-ganata.ads s-garopt.ads s-garpar.ads s-garpar.adb s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartab.ads s-gartab.adb s-gartyp.ads s-garuti.ads
s-garpri.o: s-garlic.ads s-garpri.ads
s-garpro.o: s-garlic.ads s-gardeb.ads s-garpro.ads s-garpro.adb s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garrem.o: s-garlic.ads s-garcon.ads s-gardeb.ads s-ganata.ads s-garnam.ads s-garpar.ads s-gaphlo.ads s-gaplsp.ads s-garpro.ads s-garrem.ads s-garrem.adb s-gasoli.ads s-gastha.ads s-garstr.ads s-garthi.ads s-gartyp.ads s-garuti.ads
s-garrep.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-garrep.ads s-garrep.adb s-gasoli.ads s-gastha.ads s-garstr.ads s-gartra.ads s-gartyp.ads s-garuti.ads
s-garser.o: s-garlic.ads s-gardeb.ads s-garopt.ads s-garrem.ads s-garser.ads s-garser.adb s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garsta.o: s-garlic.ads s-gardeb.ads s-garela.ads s-garfil.ads s-gargro.ads s-garhea.ads s-ganata.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gaprco.ads s-garser.ads s-gasoli.ads s-garsta.ads s-garsta.adb s-gastha.ads s-garstr.ads s-gartra.ads s-gartyp.ads s-garuni.ads s-garuti.ads
s-garstr.o: s-garlic.ads s-gardeb.ads s-gasoli.ads s-gastha.ads s-gastha.adb s-garstr.ads s-garstr.adb s-garuti.ads
s-gartab.o: s-garlic.ads s-gardeb.ads s-ganata.ads s-gasoli.ads s-gartab.ads s-gartab.adb s-garuti.ads
s-gartcp.o: s-garlic.ads s-garcon.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garnam.ads s-ganeut.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpri.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartab.ads s-gartab.adb s-gartcp.ads s-gartcp.adb s-gatcop.ads s-gtplsp.ads s-garthi.ads s-gartyp.ads s-garuti.ads
s-garter.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-garter.ads s-garter.adb s-gartyp.ads s-garuti.ads
s-garthi.o: s-garlic.ads s-garthi.ads
s-gartra.o: s-garlic.ads s-gardeb.ads s-garopt.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartra.ads s-gartra.adb s-gartyp.ads s-garuti.ads
s-gartyp.o: s-garlic.ads s-gardeb.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garuni.o: s-garlic.ads s-gardeb.ads s-gargro.ads s-garhea.ads s-ganata.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartab.ads s-gartab.adb s-gartyp.ads s-garuni.ads s-garuni.adb s-garuti.ads
s-garuti.o: s-garlic.ads s-gardeb.ads s-garuti.ads s-garuti.adb
s-gasoli.o: s-garlic.ads s-gardeb.ads s-gasoli.ads s-gasoli.adb s-garuti.ads
s-gatcop.o: s-garlic.ads s-gardeb.ads s-garpro.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartcp.ads s-gatcop.ads s-garthi.ads s-gartyp.ads s-garuti.ads
s-gtplsp.o: s-garlic.ads s-gtplsp.ads
s-parint.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garopt.ads s-garpar.ads s-gaphlo.ads s-garpro.ads s-garrem.ads s-gasoli.ads s-garsta.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuni.ads s-garuti.ads s-parint.ads s-parint.adb s-rpc.ads
s-rpc.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-gasoli.ads s-garsta.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuni.ads s-garuti.ads s-rpc.ads s-rpc.adb s-rpcpoo.ads s-rpstio.ads
s-rpcpoo.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-garopt.ads s-garpri.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads s-rpc.ads s-rpcpoo.ads s-rpcpoo.adb
s-rpstio.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-gasoli.ads s-gastha.ads s-garstr.ads s-gartab.ads s-gartab.adb s-gartyp.ads s-garuti.ads s-rpc.ads s-rpstio.ads s-rpstio.adb
s-stratt.o: s-stratt.adb
