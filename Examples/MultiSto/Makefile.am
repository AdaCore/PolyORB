##
## Makefile.am,v 1.1 1996/04/10 15:41:42 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

GNATDIST = gnatdist

EXTRA_PROGS = main
CFGFILE = multisto.cfg
PARTITIONS = part1 part2

CLEANFILES = -r dsa *.o *.ali bin $(PARTITIONS)
DISTCLEANFILES = $(CLEANFILES)

all-local: $(top_srcdir)/Examples/check-stamp $(EXTRA_PROGS)

$(top_srcdir)/Examples/check-stamp:
	IN_MAKE=yes $(SHELL) $(top_srcdir)/Examples/check-config
	echo "It worked on `date`" > $(top_srcdir)/Examples/check-stamp

$(EXTRA_PROGS):
	$(GNATDIST) -a -I$(srcdir) $(CFGFILE)
