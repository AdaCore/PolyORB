##
## Makefile.am,v 1.11 1996/05/19 14:03:21 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

CC      = @CC@
SED     = @SED@
RANLIB  = @RANLIB@

CONSTANTS_SH = ${srcdir}/constants.sh
CP           = cp
GARLIC       = ${prefix}/lib/garlic
GNATFLAGS    = -gnatg @GNATFLAGS@
INCFLAGS           = -I. -I${srcdir}

GNATMAKE     = gnatmake

PLATFORM           = @PLATFORM_SPECIFIC@

TCP_PLATFORM       = 5${PLATFORM}gtplsp.ads
CONSTANTS_PLATFORM = ${srcdir}/5${PLATFORM}garcon.ads

THIN_PLATFORM      = ${srcdir}/5${PLATFORM}garthi.ads
THIN_GENERIC       = ${srcdir}/5lgarthi.ads

EXTRA_LIBS = @EXTRA_LIBS@

LAUNCHINGOBJECTS = \
s-gaplsp.o\
s-garnam.o\
s-garrem.o

LAUNCHINGSOURCES = \
        ./s-gaplsp.ads                        \
${srcdir}/s-garnam.ads ${srcdir}/s-garnam.adb \
${srcdir}/s-garrem.ads ${srcdir}/s-garrem.adb

TCPOBJECTS = \
s-gtplsp.o\
s-gartcp.o

TCPSOURCES = \
        ./s-gtplsp.ads                        \
${srcdir}/s-gartcp.ads ${srcdir}/s-gartcp.adb

NOBLTCPOBJECTS = \
s-ganobl.o

NOBLTCPSOURCES = \
${srcdir}/s-ganobl.ads ${srcdir}/s-ganobl.adb

Makefile: Makefile.fil

@FILTERFILE@

COMMONOBJECTS = \
s-galiop.o\
s-ganata.o\
s-garthi.o\
s-gaphlo.o\
s-garcon.o\
s-gardeb.o\
s-garfil.o\
s-garhea.o\
s-garlic.o\
s-garopt.o\
s-garela.o\
s-garpri.o\
s-garpro.o\
s-garrep.o\
s-garser.o\
s-garsta.o\
s-gartra.o\
s-garloo.o\
s-gaprco.o\
s-gartab.o\
s-garstr.o\
s-garter.o\
s-gartyp.o\
s-garuni.o\
s-garuti.o\
s-painst.o\
s-parint.o\
s-rpc.o\
s-rpcini.o\
s-rpcpoo.o\
s-rpstio.o\
s-stratt.o

COMMONSOURCES = \
${srcdir}/s-galiop.ads \
${srcdir}/s-garthi.ads \
${srcdir}/s-gaphlo.ads ${srcdir}/s-gaphlo.adb \
        ./s-garcon.ads                        \
${srcdir}/s-gardeb.ads ${srcdir}/s-gardeb.adb \
${srcdir}/s-garfil.ads ${srcdir}/s-garfil.adb \
${srcdir}/s-garhea.ads ${srcdir}/s-garhea.adb \
${srcdir}/s-garlic.ads                        \
${srcdir}/s-garloo.ads ${srcdir}/s-garloo.adb \
${srcdir}/s-garopt.ads ${srcdir}/s-garopt.adb \
${srcdir}/s-garpro.ads                        \
${srcdir}/s-garpri.ads                        \
${srcdir}/s-ganata.ads ${srcdir}/s-ganata.adb \
${srcdir}/s-garpro.ads ${srcdir}/s-garpro.adb \
${srcdir}/s-garrep.ads ${srcdir}/s-garrep.adb \
${srcdir}/s-garser.ads           s-garser.adb \
${srcdir}/s-garsta.ads ${srcdir}/s-garsta.adb \
${srcdir}/s-garstr.ads $(srcdir)/s-garstr.adb \
${srcdir}/s-gartra.ads ${srcdir}/s-gartra.adb \
${srcdir}/s-gaprco.ads           s-gaprco.adb \
${srcdir}/s-garela.ads ${srcdir}/s-garela.adb \
${srcdir}/s-garter.ads ${srcdir}/s-garter.adb \
${srcdir}/s-gartab.ads ${srcdir}/s-gartab.adb \
${srcdir}/s-gartyp.ads ${srcdir}/s-gartyp.adb \
${srcdir}/s-garuni.ads ${srcdir}/s-garuni.adb \
${srcdir}/s-garuti.ads ${srcdir}/s-garuti.adb \
${srcdir}/s-gastha.ads ${srcdir}/s-gastha.adb \
${srcdir}/s-parint.ads ${srcdir}/s-parint.adb \
${srcdir}/s-painst.ads ${srcdir}/s-painst.adb \
${srcdir}/s-rpc.ads    ${srcdir}/s-rpc.adb    \
${srcdir}/s-rpcini.ads ${srcdir}/s-rpcini.adb \
${srcdir}/s-rpcpoo.ads ${srcdir}/s-rpcpoo.adb \
${srcdir}/s-rpstio.ads ${srcdir}/s-rpstio.adb \
${srcdir}/s-stratt.adb

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

CLEANFILES = *.ali s-garcon.ads *.o s-gtplsp.ads s-gaprco.adb s-galiop.ads\
	s-garser.adb s-gaplsp.ads $(FILTERSOBJECTS) split s-rpcdum.ads\
	inlining gnatlib/*

SUFFIXES = .adb .ads .ali .o .xrb

%.o: %.adb
	${CC} -c ${CFLAGS} ${INCFLAGS} ${GNATFLAGS} $<

%.o: %.ads
	${CC} -c ${CFLAGS} ${INCFLAGS} ${GNATFLAGS} $<


%.ali: %.adb
	${CC} -c ${CFLAGS} ${INCFLAGS} ${GNATFLAGS} $<


%.ali: %.ads
	${CC} -c ${CFLAGS} ${INCFLAGS} ${GNATFLAGS} $<

s-garcon.ads: ${CONSTANTS_SH} split
	if test -f ${CONSTANTS_PLATFORM}; then \
	  cp ${CONSTANTS_PLATFORM} s-garcon.ads; \
	else \
	  ${SHELL} ${CONSTANTS_SH} System.Garlic.Constants; \
	fi

s-galiop.ads:
	@echo "private package System.Garlic.Linker_Options is" >s-galiop.ads
	@echo "" >>s-galiop.ads
	@if [ "x$(EXTRA_LIBS)" != "x" ] ; then \
	 for option in `echo $(EXTRA_LIBS)`; do \
	  echo "   pragma Linker_Options (\"$$option\");" >>s-galiop.ads; \
	 done; \
	fi
	@echo "" >>s-galiop.ads
	@echo "end System.Garlic.Linker_Options;" >>s-galiop.ads

s-gtplsp.ads: ${srcdir}/${TCP_PLATFORM}
	cp ${srcdir}/${TCP_PLATFORM} s-gtplsp.ads

s-garthi.ads: Makefile
	if test -f ${THIN_PLATFORM}; then \
	  cp ${THIN_PLATFORM} s-garthi.ads; \
	else \
	  cp ${THIN_GENERIC} s-garthi.ads; \
	fi

split: ${srcdir}/split.adb
	$(GNATMAKE) -m -o split ${srcdir}/split.adb

all-local: ${SOURCES} ${OBJECTS}

fast: ${SOURCES} s-rpcdum.ads
	@gnatmake -m -c -a ${CFLAGS} ${INCFLAGS} ${GNATFLAGS} s-rpcdum

s-rpcdum.ads: ${SOURCES} Makefile
	@grep "^package " ${SOURCES} | \
	${AWK} '{if ($$2 != "body") {print "with " $$2 ";" } else {print "with " $$3 ";" }}' | \
	sort -u > s-rpcdum.ads
	@echo "private package System.RPC.Dummy is" >> s-rpcdum.ads
	@echo "end System.RPC.Dummy;" >> s-rpcdum.ads

install-dirs-local:
	-rm -rf ${GARLIC}
	${top_srcdir}/mkinstalldirs ${GARLIC}

inlining: all-local
	@echo 'with Text_IO; use Text_IO;'  >inline.adb
	@echo 'procedure Inline is'        >>inline.adb
	@echo 'begin Put_Line ("a"); end;' >>inline.adb
	-@${GNATMAKE} -I. inline.adb
	@if test -f inline; then\
	   echo no >inlining;\
	   echo "Run-time compiled without inlining flags";\
	else\
	   echo "Run-time compiled with inlining flags";\
	fi;
	${RM} -f inline.adb inline.o inline.ali inline
	@if test ! -f inlining; then\
	   ${RM} -r gnatlib;\
	   mkdir gnatlib;\
	   gnatlibdir=`${srcdir}/../Dist/config.sdefault pathtolib`; \
	   for o in `cd $$gnatlibdir && echo *.ali`; do \
	     grep s-stratt $$gnatlibdir/$$o /dev/null; \
	   done | ${SED} 's,^.*[/]\(.*\)\.ali:.*$$,\1.o,g' > gnatlib/tocheck; \
	   for o in `cat gnatlib/tocheck`; do\
	      if [ ! -f $$o ]; then \
	          s=`gnatls -a -s $$o`; \
	          echo "${CC} -c ${CFLAGS} ${GNATFLAGS} -I. -I- $$s" \
	               "-o gnatlib/$$o"; \
	          ${CC} -c ${CFLAGS} ${GNATFLAGS} -I. -I- $$s -o gnatlib/$$o; \
	          echo $$o >>gnatlib/rebuild; \
	      fi;\
	   done;\
	   echo yes >inlining;\
	fi

install-extra-libs: ${FILTERINSTALL} inlining

install-data-local: all-local install-dirs-local install-extra-libs 
	${INSTALL}\
	   `${srcdir}/../Dist/config.sdefault pathtolib`libgnat.a ${GARLIC}
	${AR} d ${GARLIC}/libgnat.a s-stratt.o s-rpc.o s-parint.o
	if test -d gnatlib; then\
	   (cd gnatlib;\
	    ${AR} cq ${GARLIC}/libgnat.a `cat rebuild`;\
            ${CP} -p `${SED} 's/\.o/.ali/g' rebuild` ${GARLIC});\
	fi
	${RANLIB} ${GARLIC}/libgnat.a
	for i in ${GARLIC} ${SOURCES} \
		`echo ${OBJECTS} | ${SED} -e 's/\.o/.ali/g'`; do \
		if test -f $$i; then \
			${CP} -p $$i ${GARLIC}/; \
		fi; \
	done
	${AR} cq ${GARLIC}/libgarlic.a ${OBJECTS}
	${RANLIB} ${GARLIC}/libgarlic.a
	chmod a+r ${GARLIC}/*
	chmod a-w ${GARLIC}/*

# Dependencies

depend: $(SOURCES)
	$(MAKE) fast
	$(srcdir)/mkdep
	$(MAKE) Makefile

# Cross references

X.ref: ${SOURCES}
	gnatf -x6 ${SOURCES}

%.xrb: %.adb
	gnatf -x5 $<

%.xrb: %.ads
	gnatf -x5 $<

###
### The following section is valid only for developpers that use the
### --enable-debug flag on configure. If you want to make modifications
### outside the development mode, you need to either update the following
### dependencies or to use the "fast" target.
###
### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
s-gafizi.o: s-garlic.ads s-gardeb.ads s-garfil.ads s-gafizi.ads s-gafizi.adb s-garhea.ads s-ganata.ads s-gaphlo.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-galiop.o: s-garlic.ads s-galiop.ads
s-ganata.o: s-garlic.ads s-gardeb.ads s-ganata.ads s-ganata.adb s-gartab.ads s-gartab.adb s-gartyp.ads s-garuti.ads
s-ganobl.o: s-garlic.ads s-garcon.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-ganobl.ads s-ganobl.adb s-gaphlo.ads s-garpri.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartcp.ads s-garter.ads s-garthi.ads s-gartyp.ads s-garuti.ads
s-gaphlo.o: s-garlic.ads s-gardeb.ads s-gaphlo.ads s-gaphlo.adb s-garpro.ads s-gartyp.ads
s-gaplsp.o: s-garlic.ads s-gaplsp.ads
s-gaprco.o: s-garlic.ads s-garloo.ads s-garpro.ads s-gaprco.ads s-gaprco.adb s-garrep.ads s-gartcp.ads s-gartyp.ads
s-garcon.o: s-garlic.ads s-garcon.ads
s-gardeb.o: s-garlic.ads s-gardeb.ads s-gardeb.adb
s-garela.o: s-garlic.ads s-gardeb.ads s-garela.ads s-garela.adb s-garhea.ads s-ganata.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garfil.o: s-garlic.ads s-gardeb.ads s-garfil.ads s-garfil.adb s-garhea.ads s-ganata.ads s-gaphlo.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartab.ads s-gartab.adb s-gartyp.ads s-garuti.ads
s-garhea.o: s-garlic.ads s-gardeb.ads s-garfil.ads s-garhea.ads s-garhea.adb s-galiop.ads s-ganata.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-gastha.ads s-garstr.ads s-garter.ads s-gartra.ads s-gartyp.ads s-garuti.ads s-rpc.ads s-rpcini.ads
s-garlic.o: s-garlic.ads
s-garloo.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-garloo.ads s-garloo.adb s-ganata.ads s-gaphlo.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garnam.o: s-garlic.ads s-garcon.ads s-garnam.ads s-garnam.adb s-garthi.ads s-gartyp.ads s-garuti.ads
s-garopt.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garopt.ads s-garopt.adb s-gaphlo.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garpri.o: s-garlic.ads s-garpri.ads
s-garpro.o: s-garlic.ads s-garpro.ads s-garpro.adb s-gartyp.ads
s-garrem.o: s-garlic.ads s-garcon.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garnam.ads s-gaphlo.ads s-gaplsp.ads s-garpro.ads s-garrem.ads s-garrem.adb s-gastha.ads s-garstr.ads s-garthi.ads s-gartyp.ads s-garuti.ads
s-garrep.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-garrep.ads s-garrep.adb s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garser.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-garrem.ads s-garser.ads s-garser.adb s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads
s-garsta.o: s-garlic.ads s-gardeb.ads s-garela.ads s-garfil.ads s-garhea.ads s-ganata.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-gaprco.ads s-garser.ads s-garsta.ads s-garsta.adb s-gastha.ads s-garstr.ads s-garter.ads s-gartra.ads s-gartyp.ads s-garuti.ads s-rpc.ads s-rpcini.ads
s-garstr.o: s-garlic.ads s-gardeb.ads s-gastha.ads s-gastha.adb s-garstr.ads s-garstr.adb s-garuti.ads
s-gartab.o: s-garlic.ads s-gardeb.ads s-ganata.ads s-gartab.ads s-gartab.adb s-garuti.ads
s-gartcp.o: s-garlic.ads s-garcon.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garnam.ads s-ganobl.ads s-garopt.ads s-gaphlo.ads s-garpri.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartcp.ads s-gartcp.adb s-gtplsp.ads s-garter.ads s-garthi.ads s-gartyp.ads s-garuti.ads
s-garter.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garopt.ads s-gaphlo.ads s-garpri.ads s-garpro.ads s-gastha.ads s-garstr.ads s-garter.ads s-garter.adb s-gartyp.ads s-garuti.ads
s-garthi.o: s-garlic.ads s-garthi.ads
s-gartra.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartra.ads s-gartra.adb s-gartyp.ads s-garuti.ads
s-gartyp.o: s-garlic.ads s-gartyp.ads s-gartyp.adb
s-garuni.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-gaphlo.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartab.ads s-gartab.adb s-gartyp.ads s-garuni.ads s-garuni.adb s-garuti.ads
s-garuti.o: s-garlic.ads s-gartyp.ads s-garuti.ads s-garuti.adb
s-gtplsp.o: s-garlic.ads s-ganobl.ads s-garpro.ads s-gartcp.ads s-gtplsp.ads s-garthi.ads s-gartyp.ads
s-painst.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-garrem.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads s-parint.ads s-painst.ads s-painst.adb s-rpc.ads
s-parint.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-gaphlo.ads s-garpro.ads s-garsta.ads s-gastha.ads s-garstr.ads s-gartab.ads s-gartyp.ads s-garuni.ads s-garuti.ads s-parint.ads s-parint.adb s-rpc.ads
s-rpc.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-gaphlo.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads s-rpc.ads s-rpc.adb s-rpcpoo.ads
s-rpcini.o: s-garlic.ads s-gardeb.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads s-rpc.ads s-rpcini.ads s-rpcini.adb s-rpcpoo.ads s-rpstio.ads
s-rpcpoo.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-garopt.ads s-gaphlo.ads s-garpri.ads s-garpro.ads s-gastha.ads s-garstr.ads s-garter.ads s-gartyp.ads s-garuti.ads s-rpc.ads s-rpcpoo.ads s-rpcpoo.adb
s-rpstio.o: s-garlic.ads s-gardeb.ads s-garhea.ads s-ganata.ads s-gaphlo.ads s-garpro.ads s-gastha.ads s-garstr.ads s-gartyp.ads s-garuti.ads s-rpc.ads s-rpstio.ads s-rpstio.adb
s-stratt.o: s-stratt.adb
split.o: split.adb
