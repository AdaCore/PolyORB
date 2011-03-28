AUTOMAKE_OPTIONS = no-dependencies foreign

SED = @SED@

GNATDIST = $(exec_prefix)/bin/gnatdist

EXTRA_PROGS = mainloop
CFGFILE = filters.cfg

CLEANFILES = -r dsa *.o *.ali bin $(EXTRA_PROGS)
DISTCLEANFILES = $(CLEANFILES)

all-local: $(top_srcdir)/Examples/check-stamp $(EXTRA_PROGS)

$(top_srcdir)/Examples/check-stamp:
	IN_MAKE=yes $(SHELL) $(top_srcdir)/Examples/check-config
	echo "It worked on `date`" > $(top_srcdir)/Examples/check-stamp

$(EXTRA_PROGS):
	$(GNATDIST) -a -I$(srcdir) $(CFGFILE)

