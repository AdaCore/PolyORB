#
# Makefile for PolyORB.
#
# $Id$
#

GNATMAKE=gnatmake --GCC=./compile

CFLAGS=		-O2 -gnatf -gnatwae -Icorba $(COPTS)
# -gnaty is position by the 'compile' script for files that support it.

# Development
CFLAGS+=	-g -gnatao

TESTS=	polyorb-test-no_tasking \
	polyorb-test-thread_pool \
	polyorb-test-no_tasking_poa \
	polyorb-test-thread_pool_poa \
	dynclient \
	server


all: $(TESTS)

clean:
	$(RM) *.ali *.o *.dot *.ps

alldeps.dot: polyorb-setup-test
	cat polyorb-*.ali | ali2dot > $@

alldeps.ps: alldeps.dot
	dot -Tps $< | epsffit -c -r 10 10 535 831 > $@

$(TESTS): force
	$(GNATMAKE) $@ -cargs $(CFLAGS) -largs $(LDFLAGS)

force:

.PHONY: force
