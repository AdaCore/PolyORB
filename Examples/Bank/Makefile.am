##
## Makefile.am,v 1.1 1996/04/10 15:41:42 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

GNATDIST = gnatdist

EXTRA_PROGS = mainloop
CFGFILE = simcity.cfg
PARTITIONS = bank_server bank_client

CLEANFILES = -r dsa *.o *.ali bin
DISTCLEANFILES = $(EXTRA_PROGS) $(PARTITIONS)

all-local: ${top_srcdir}/Examples/check-stamp ${EXTRA_PROGS}

${top_srcdir}/Examples/check-stamp:
	IN_MAKE=yes ${SHELL} ${top_srcdir}/Examples/check-config
	echo "It worked on `date`" > ${top_srcdir}/Examples/check-stamp

$(PARTITIONS):
	${GNATDIST} -I${srcdir} $(CFGFILE)

$(EXTRA_PROGS): $(PARTITIONS)

