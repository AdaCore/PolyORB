#
# Makefile for DROOPI.
#
# $Id$
#

GNATMAKE=gnatmake

CFLAGS=		-O2 -gnatfy -gnatwae -Icorba $(COPTS)

# Development
CFLAGS+=	-g -gnatao

all: droopi-setup-test

clean:
	$(RM) *.ali *.o *.dot *.ps

alldeps.dot: droopi-setup-test
	cat droopi-*.ali | ali2dot > $@

alldeps.ps: alldeps.dot
	dot -Tps $< | epsffit -c -r 10 10 535 831 > $@

droopi-setup-test: force
	$(GNATMAKE) droopi-setup-test -cargs $(CFLAGS)

force:

.PHONY: force
