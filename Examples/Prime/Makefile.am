##
## Makefile.am,v 1.1 1996/04/10 15:41:42 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

SED = @SED@

GNATDIST = ${exec_prefix}/bin/gnatdist

EXTRA_PROGS = master

CLEANFILES = -r dsa *.o *.ali
DISTCLEANFILES = master51.cfg master part1 part2 part3 part4

all-local: check-config ${EXTRA_PROGS}

check-config: 
	IN_MAKE=yes ${SHELL} ../check-config

bin/part1 bin/part2 sub/bin/part3 bin/part4:

master: master51.cfg bin/part1 bin/part2 sub/bin/part3 bin/part4
	${GNATDIST} -I${srcdir} master51.cfg

master51.cfg: master51.cfg.in
	${SED} -e "s,XXXXX,`pwd`,g" < ${srcdir}/master51.cfg.in > master51.cfg

