#
# Makefile,v 1.5 1996/04/16 15:25:42 tardieu Exp
#

SHELL = /bin/sh

all:: test_basic

never::

test_basic: never t_b.cfg
	gnatdist -v -m -O2 -g t_b.cfg

t_b.cfg: t_b.cfg.in
	p=`pwd`; h=`hostname`; sed -e "s,YYYYY,$$h," \
		< t_b.cfg.in > t_b.cfg

clean: 
	${RM} -r *.o *.ali dsa test_basic.cfg

distclean: clean
	${RM} -r bin test_basic
