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
s-rpc.o

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
${srcdir}/s-gtplsp.ads                        \
${srcdir}/s-parint.ads ${srcdir}/s-parint.adb \
${srcdir}/s-partyp.ads                        \
${srcdir}/s-rpc.ads    ${srcdir}/s-rpc.adb

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
s-ganobl.o: s-ganobl.adb s-garlic.ads s-garcon.ads s-gardeb.adb s-garhea.adb s-garpri.ads s-garter.adb s-ganobl.ads s-garlic.ads s-garthi.ads
s-gaphlo.o: s-gaphlo.adb s-gaphlo.ads s-garlic.ads s-garpro.adb s-rpc.adb
s-garcon.o: s-garcon.ads s-garlic.ads
s-gardeb.o: s-gardeb.adb s-gardeb.ads s-garlic.ads
s-garhea.o: s-garhea.adb s-garlic.ads s-gardeb.adb s-garpro.adb s-garter.adb s-garuti.adb s-garhea.ads s-garlic.ads s-gaphlo.adb s-rpc.adb
s-garlic.o: s-garlic.ads
s-garnam.o: s-garnam.adb s-garlic.ads s-garcon.ads s-garnam.ads s-garlic.ads s-garthi.ads
s-garopt.o: s-garopt.adb s-garlic.ads s-gardeb.adb s-garopt.ads s-garlic.ads
s-garpri.o: s-garpri.ads s-garlic.ads
s-garpro.o: s-garpro.adb s-garpro.ads s-garlic.ads s-rpc.adb
s-garrem.o: s-garrem.adb s-garlic.ads s-garcon.ads s-garthi.ads s-rpc.adb s-garrem.ads s-garlic.ads
s-garsta.o: s-garsta.adb s-garlic.ads s-gardeb.adb s-garhea.adb s-garopt.adb s-gaphlo.adb s-garpro.adb s-garrem.adb s-gartcp.adb s-garter.adb s-rpc.adb s-garsta.ads s-garlic.ads
s-gartcp.o: s-gartcp.adb s-garlic.ads s-garcon.ads s-gardeb.adb s-garhea.adb s-garnam.adb s-garopt.adb s-gaphlo.adb s-garpri.ads s-gartcp.adb s-gtplsp.ads s-garter.adb s-garthi.ads s-garuti.adb s-gartcp.ads s-garlic.ads s-garpro.adb s-rpc.adb
s-garter.o: s-garter.adb s-garlic.ads s-gardeb.adb s-garhea.adb s-garpri.ads s-garuti.adb s-rpc.adb s-garter.ads s-garlic.ads s-rpc.adb
s-garthi.o: s-garthi.ads s-garlic.ads s-garcon.ads
s-garuti.o: s-garuti.adb s-garuti.ads s-garlic.ads s-rpc.adb
s-gtplsp.o: s-gtplsp.ads s-garlic.ads s-gartcp.adb s-garthi.ads
s-parint.o: s-parint.adb s-garlic.ads s-gardeb.adb s-garhea.adb s-garter.adb s-rpc.adb s-parint.ads s-partyp.ads s-rpc.adb
s-partyp.o: s-partyp.ads s-rpc.adb
s-rpc.o: s-rpc.adb s-garlic.ads s-gardeb.adb s-garhea.adb s-garter.adb s-rpc.ads
