##
## Makefile.am,v 1.1 1996/04/10 15:41:42 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

SED = @SED@

GNATDIST = $(exec_prefix)/bin/gnatdist

EXTRA_PROGS = main
CFGFILE = reconnection.cfg

CLEANFILES = -r dsa *.o *.ali bin $(EXTRA_PROGS)
DISTCLEANFILES = $(CLEANFILES)

all-local: ../check-stamp $(EXTRA_PROGS)

../check-stamp:
	IN_MAKE=yes $(SHELL) ../check-config
	echo "It worked on `date`" > ../check-stamp

$(EXTRA_PROGS):
	$(GNATDIST) -I$(srcdir) $(CFGFILE)

