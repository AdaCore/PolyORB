#
# Makefile,v 1.3 1996/04/16 15:25:46 tardieu Exp
#

SHELL = /bin/sh

all: test_exceptions

never: 

test_exceptions: never
	gnatdist -g -v te.cfg -cargs -gnato -gnata

clean: 
	${RM} -r *.o *.ali dsa

distclean: clean
	${RM} -r bin test_exceptions
