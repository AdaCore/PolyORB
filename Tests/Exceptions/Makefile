#
# Makefile,v 1.3 1996/04/16 15:25:46 tardieu Exp
#

SHELL = /bin/sh

all: test_exceptions

never: 

test_exceptions: never test_exceptions.cfg
	gnatdist -g -v test_exceptions.cfg -cargs -gnato -gnata

test_exceptions.cfg: test_exceptions.cfg.in
	p=`pwd`; h=`hostname`; sed -e "s,XXXXX,$$p," -e "s,YYYYY,$$h," \
		< test_exceptions.cfg.in > test_exceptions.cfg

clean: 
	${RM} -r *.o *.ali dsa test_exceptions.cfg

distclean: clean
	${RM} -r bin test_exceptions
