##
## Makefile.am,v 1.1 1996/04/10 15:41:42 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

SED = @SED@

GNATDIST = ${exec_prefix}/bin/gnatdist

EXTRA_PROGS = mainloop
CFGFILE = absolute.cfg
PARTITIONS = partition1 partition2 partition3

CLEANFILES = -r dsa *.o *.ali
DISTCLEANFILES = $(EXTRA_PROGS) $(PARTITIONS)

all-local: ../../check-stamp ${EXTRA_PROGS}

../../check-stamp:
	IN_MAKE=yes ${SHELL} ../../check-config
	echo "It worked on `date`" > ../../check-stamp

$(PARTITIONS):
	${GNATDIST} -I${srcdir} $(CFGFILE)

$(EXTRA_PROGS): $(PARTITIONS)

