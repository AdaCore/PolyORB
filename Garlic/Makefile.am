##
## Makefile.am,v 1.11 1996/05/19 14:03:21 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

CC      = @CC@
SED     = @SED@

CONSTANTS_SH = ${srcdir}/constants.sh
CP           = cp
GARLIC       = ${prefix}/lib/garlic
GNATFLAGS    = -gnatg -gnato @GNATFLAGS@
INCFLAGS     = -I. -I${srcdir}

PLATFORM     = @PLATFORM_SPECIFIC@

TCP_PLATFORM = 2${PLATFORM}gtplsp.ads

OBJECTS = \
s-gaphlo.o\
s-garcon.o\
s-gardeb.o\
s-garhea.o\
s-garlic.o\
s-garnam.o\
s-ganobl.o\
s-garopt.o\
s-garpri.o\
s-garpro.o\
s-garrem.o\
s-garsta.o\
s-gartcp.o\
s-garter.o\
s-garthi.o\
s-garuti.o\
s-gtplsp.o\
s-parint.o\
s-partyp.o\
s-rpc.o\
s-stratt.o

SOURCES = \
${srcdir}/rci_info.ads ${srcdir}/rci_info.adb \
${srcdir}/s-gaphlo.ads ${srcdir}/s-gaphlo.adb \
${srcdir}/s-garcac.ads ${srcdir}/s-garcac.adb \
        ./s-garcon.ads                        \
${srcdir}/s-gardeb.ads ${srcdir}/s-gardeb.adb \
${srcdir}/s-garhea.ads ${srcdir}/s-garhea.adb \
${srcdir}/s-garlic.ads                        \
${srcdir}/s-garnam.ads ${srcdir}/s-garnam.adb \
${srcdir}/s-ganobl.ads ${srcdir}/s-ganobl.adb \
${srcdir}/s-garopt.ads ${srcdir}/s-garopt.adb \
${srcdir}/s-garpro.ads                        \
${srcdir}/s-garpoi.ads ${srcdir}/s-garpoi.adb \
${srcdir}/s-garpro.ads ${srcdir}/s-garpro.adb \
${srcdir}/s-garrem.ads ${srcdir}/s-garrem.adb \
${srcdir}/s-garsta.ads ${srcdir}/s-garsta.adb \
${srcdir}/s-gartcp.ads ${srcdir}/s-gartcp.adb \
${srcdir}/s-garter.ads ${srcdir}/s-garter.adb \
${srcdir}/s-garthi.ads                        \
${srcdir}/s-garuti.ads ${srcdir}/s-garuti.adb \
        ./s-gtplsp.ads                        \
${srcdir}/s-parint.ads ${srcdir}/s-parint.adb \
${srcdir}/s-partyp.ads                        \
${srcdir}/s-rpc.ads    ${srcdir}/s-rpc.adb    \
${srcdir}/s-stratt.adb

CLEANFILES = *.ali s-garcon.ads *.o s-gtplsp.ads

.SUFFIXES:	.adb .ads .ali .o .xrb

%.o: %.adb
	${CC} -c ${CFLAGS} ${INCFLAGS} ${GNATFLAGS} $<

%.o: %.ads
	${CC} -c ${CFLAGS} ${INCFLAGS} ${GNATFLAGS} $<


%.ali: %.adb
	${CC} -c ${CFLAGS} ${INCFLAGS} ${GNATFLAGS} $<


%.ali: %.ads
	${CC} -c ${CFLAGS} ${INCFLAGS} ${GNATFLAGS} $<

s-garcon.ads: ${CONSTANTS_SH}
	${SHELL} ${CONSTANTS_SH} System.Garlic.Constants

s-gtplsp.ads: ${srcdir}/${TCP_PLATFORM}
	cp ${srcdir}/${TCP_PLATFORM} s-gtplsp.ads

all-local: ${OBJECTS} ${SOURCES}

install-data-local: all-local
	-rm -rf ${GARLIC}
	${top_srcdir}/mkinstalldirs ${GARLIC}
	${CP} -p ${SOURCES} ${OBJECTS} \
		`echo ${OBJECTS} | sed -e 's/\.o/.ali/g'` ${GARLIC}

# Cross references

X.ref: ${SOURCES}
	gnatf -x6 ${SOURCES}

%.xrb: %.adb
	gnatf -x5 $<

%.xrb: %.ads
	gnatf -x5 $<

# Special dependencies

s-garnam.o: s-garcon.ads
s-garrem.o: s-garcon.ads
s-garsta.o: s-garcon.ads s-gtplsp.ads
s-gartcp.o: s-garcon.ads s-gtplsp.ads
s-garthi.o: s-garcon.ads

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
s-ganobl.o: s-garlic.ads s-garcon.ads s-gardeb.ads s-gardeb.adb s-garhea.ads s-ganobl.ads s-ganobl.adb s-gaphlo.ads s-garpoi.ads s-garpri.ads s-garpro.ads s-garter.ads s-garter.adb s-garthi.ads s-garuti.ads s-rpc.ads
s-gaphlo.o: s-garlic.ads s-gaphlo.ads s-gaphlo.adb s-garpro.ads s-rpc.ads s-stratt.adb
s-garcon.o: s-garlic.ads s-garcon.ads
s-gardeb.o: s-garlic.ads s-gardeb.ads s-gardeb.adb
s-garhea.o: s-garlic.ads s-garcac.ads s-garcon.ads s-gardeb.ads s-gardeb.adb s-garhea.ads s-garhea.adb s-gaphlo.ads s-garpoi.ads s-garpri.ads s-garpro.ads s-garter.ads s-garter.adb s-garthi.ads s-garuti.ads s-garuti.adb s-rpc.ads s-rpc.adb s-stratt.adb
s-garlic.o: s-garlic.ads
s-garnam.o: s-garlic.ads s-garcon.ads s-garnam.ads s-garnam.adb s-garpoi.ads s-garthi.ads s-stratt.adb
s-garopt.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garopt.ads s-garopt.adb
s-garpri.o: s-garlic.ads s-garpri.ads
s-garpro.o: s-garlic.ads s-garpro.ads s-garpro.adb s-rpc.ads
s-garrem.o: s-garlic.ads s-garcon.ads s-garpoi.ads s-garrem.ads s-garrem.adb s-garthi.ads s-rpc.ads
s-garsta.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garhea.ads s-garopt.ads s-gaphlo.ads s-garpro.ads s-garrem.ads s-garsta.ads s-garsta.adb s-gartcp.ads s-garter.ads s-rpc.ads
s-gartcp.o: s-garlic.ads s-garcac.ads s-garcon.ads s-gardeb.ads s-gardeb.adb s-garhea.ads s-garhea.adb s-garnam.ads s-garopt.ads s-gaphlo.ads s-garpoi.ads s-garpri.ads s-garpro.ads s-gartcp.ads s-gartcp.adb s-gtplsp.ads s-garter.ads s-garter.adb s-garthi.ads s-garuti.ads s-garuti.adb s-rpc.ads s-rpc.adb s-stratt.adb
s-garter.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garhea.ads s-gaphlo.ads s-garpri.ads s-garpro.ads s-garter.ads s-garter.adb s-garuti.ads s-rpc.ads s-stratt.adb
s-garthi.o: s-garlic.ads s-garcon.ads s-garpoi.ads s-garpoi.adb s-garthi.ads
s-garuti.o: s-garlic.ads s-garcac.ads s-gardeb.ads s-garhea.ads s-gaphlo.ads s-garpro.ads s-garter.ads s-garuti.ads s-garuti.adb s-rpc.ads s-rpc.adb
s-gtplsp.o: s-garlic.ads s-garcon.ads s-garpoi.ads s-garpro.ads s-gartcp.ads s-gtplsp.ads s-garthi.ads s-rpc.ads
s-parint.o: s-garlic.ads s-gardeb.ads s-gardeb.adb s-garhea.ads s-gaphlo.ads s-garpri.ads s-garpro.ads s-garsta.ads s-garter.ads s-garter.adb s-garuti.ads s-parint.ads s-parint.adb s-partyp.ads s-rpc.ads s-stratt.adb
s-partyp.o: s-partyp.ads s-rpc.ads
s-rpc.o: s-garlic.ads s-garcac.ads s-garcac.adb s-gardeb.ads s-gardeb.adb s-garhea.ads s-gaphlo.ads s-garpri.ads s-garpro.ads s-garter.ads s-garter.adb s-garuti.ads s-rpc.ads s-rpc.adb s-stratt.adb
s-stratt.o: s-stratt.adb
