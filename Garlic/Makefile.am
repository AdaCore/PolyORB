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

LAUNCHING_OBJECTS = \
s-garnam.o\
s-garrem.o

TCP_OBJECTS = \
s-ganobl.o\
s-gtplsp.o\
s-gartcp.o\
s-garthi.o

COMMON_OBJECTS = \
s-gaphlo.o\
s-garcon.o\
s-garser.o\
s-gardeb.o\
s-garhea.o\
s-garlic.o\
s-garopt.o\
s-garpri.o\
s-garpro.o\
s-garsta.o\
s-garloo.o\
s-gaprco.o\
s-garter.o\
s-garuti.o\
s-parint.o\
s-partyp.o\
s-rpc.o\
s-stratt.o

OBJECTS = $(COMMON_OBJECTS) @PROTOCOLS_OBJECTS@ @LAUNCHING_OBJECTS@

LAUNCHING_SOURCES = \
${srcdir}/s-garnam.ads ${srcdir}/s-garnam.adb \
${srcdir}/s-garrem.ads ${srcdir}/s-garrem.adb

TCP_SOURCES = \
${srcdir}/s-ganobl.ads ${srcdir}/s-ganobl.adb \
        ./s-gtplsp.ads                        \
${srcdir}/s-gartcp.ads ${srcdir}/s-gartcp.adb \
${srcdir}/s-garthi.ads

COMMON_SOURCES = \
${srcdir}/rci_info.ads ${srcdir}/rci_info.adb \
${srcdir}/s-gaphlo.ads ${srcdir}/s-gaphlo.adb \
${srcdir}/s-garcac.ads ${srcdir}/s-garcac.adb \
        ./s-garcon.ads                        \
${srcdir}/s-gardeb.ads ${srcdir}/s-gardeb.adb \
${srcdir}/s-garhea.ads ${srcdir}/s-garhea.adb \
${srcdir}/s-garser.ads ${srcdir}/s-garser.adb \
${srcdir}/s-garlic.ads                        \
${srcdir}/s-garopt.ads ${srcdir}/s-garopt.adb \
${srcdir}/s-garpro.ads                        \
${srcdir}/s-garpoi.ads ${srcdir}/s-garpoi.adb \
${srcdir}/s-garpro.ads ${srcdir}/s-garpro.adb \
${srcdir}/s-garsta.ads ${srcdir}/s-garsta.adb \
${srcdir}/s-garloo.ads ${srcdir}/s-garloo.adb \
${srcdir}/s-gaprco.ads ${srcdir}/s-gaprco.adb \
${srcdir}/s-garter.ads ${srcdir}/s-garter.adb \
${srcdir}/s-garuti.ads ${srcdir}/s-garuti.adb \
${srcdir}/s-parint.ads ${srcdir}/s-parint.adb \
${srcdir}/s-partyp.ads                        \
${srcdir}/s-rpc.ads    ${srcdir}/s-rpc.adb    \
${srcdir}/s-stratt.adb

SOURCES = $(COMMON_SOURCES) @PROTOCOLS_SOURCES@ @LAUNCHING_SOURCES@

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

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
