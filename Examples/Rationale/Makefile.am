##
## Makefile.am,v 1.1 1996/04/10 15:41:47 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

SED = @SED@

GNATDIST = $(exec_prefix)/bin/gnatdist

EXTRA_PROGS = driver

CLEANFILES = -r dsa *.o *.ali
DISTCLEANFILES = driver client server

all-local: check-config $(EXTRA_PROGS)

check-config: 
	IN_MAKE=yes $(SHELL) ../check-config

main: 
	$(GNATDIST) -I$(srcdir) $(srcdir)/rational.cfg

