#
# Makefile for DROOPI.
#
# $Id$
#

GNATMAKE=gnatmake

CFLAGS=-O2 -gnatfy -gnatwa -Icorba

# Development
CFLAGS+=-g -gnatao

all: droopi-setup-test

clean:
	$(RM) *.ali *.o

droopi-setup-test: force
	$(GNATMAKE) droopi-setup-test -cargs $(CFLAGS)

force:

.PHONY: force
