#
# Makefile,v 1.1 1996/02/27 12:16:35 tardieu Exp
#

SHELL = /bin/sh

all: test_abort

never: 

test_abort: never test_abort.cfg
	gnatdist -v test_abort.cfg -cargs -gnato -O

test_abort.cfg: test_abort.cfg.in
	p=`pwd`; h=`hostname`; sed -e "s,XXXXX,$$p," -e "s,YYYYY,$$h," \
		< test_abort.cfg.in > test_abort.cfg

clean: 
	${RM} -r *.o *.ali dsa test_abort.cfg

distclean: clean
	${RM} -r bin test_abort
