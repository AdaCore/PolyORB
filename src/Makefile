#
# Makefile for DROOPI.
#
# $Id$
#

GNATMAKE=gnatmake --GCC=./compile

CFLAGS=		-O2 -gnatfy -gnatwae -Icorba $(COPTS)

# Development
CFLAGS+=	-g -gnatao

TESTS=	droopi-test-no_tasking \
	droopi-test-thread_pool \
	droopi-test-no_tasking_poa \
	droopi-test-thread_pool_poa \
	dynclient \
	server


all: $(TESTS)

clean:
	$(RM) *.ali *.o *.dot *.ps

alldeps.dot: droopi-setup-test
	cat droopi-*.ali | ali2dot > $@

alldeps.ps: alldeps.dot
	dot -Tps $< | epsffit -c -r 10 10 535 831 > $@

$(TESTS): force
	$(GNATMAKE) $@ -cargs $(CFLAGS) -largs $(LDFLAGS)

force:

.PHONY: force
