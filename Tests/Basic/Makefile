#
# Makefile,v 1.5 1996/04/16 15:25:42 tardieu Exp
#

SHELL = /bin/sh

all:: test_basic

never::

test_basic: never test_basic.cfg
	gnatdist -v -g test_basic.cfg -cargs -gnato -gnata

test_basic.cfg: test_basic.cfg.in
	p=`pwd`; h=`hostname`; sed -e "s,XXXXX,$$p," -e "s,YYYYY,$$h," \
		< test_basic.cfg.in > test_basic.cfg

clean: 
	${RM} -r *.o *.ali dsa test_basic.cfg

distclean: clean
	${RM} -r bin test_basic
