#
# Makefile for DROOPI.
#
# $Id$
#

GNATMAKE=gnatmake

CFLAGS=-O2 -gnatfy -gnatwae -Icorba

# Development
CFLAGS+=-g -gnatao

all: droopi-setup-test

clean:
	$(RM) *.ali *.o *.dot *.ps

alldeps.dot: droopi-setup-test
	cat droopi-*.ali | ali2dot > $@

alldeps.ps: alldeps.dot
	dot -Tps $< | epsffit -r 10 10 831 535 > $@

droopi-setup-test: force
	$(GNATMAKE) droopi-setup-test -cargs $(CFLAGS)

force:

.PHONY: force
