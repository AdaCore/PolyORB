AUTOMAKE_OPTIONS = no-dependencies foreign

GNATDIST = gnatdist

EXTRA_PROGS = bank_client
CFGFILE = simcity.cfg
PARTITIONS = bank_server bank_client

CLEANFILES = -r dsa *.o *.ali bin $(PARTITIONS)
DISTCLEANFILES = $(CLEANFILES)

all-local: $(top_srcdir)/Examples/check-stamp $(EXTRA_PROGS)

$(top_srcdir)/Examples/check-stamp:
	IN_MAKE=yes $(SHELL) $(top_srcdir)/Examples/check-config
	echo "It worked on `date`" > $(top_srcdir)/Examples/check-stamp

$(EXTRA_PROGS):
	$(GNATDIST) -I$(srcdir) $(CFGFILE)
