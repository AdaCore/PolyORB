#
# Makefile,v 1.1 1996/02/27 12:16:35 tardieu Exp
#

SHELL = /bin/sh

all: test_abort

never: 

test_abort: never ta.cfg
	gnatdist -v ta.cfg -cargs -gnato -O

ta.cfg: ta.cfg.in
	p=`pwd`; h=`hostname`; sed -e "s,XXXXX,$$p," -e "s,YYYYY,$$h," \
		< ta.cfg.in > ta.cfg

clean: 
	${RM} -r *.o *.ali dsa ta.cfg

distclean: clean
	${RM} -r bin test_abort
