all-local: tcprelay listener multi

tcprelay: never
	$(GNATMAKE) -m -A../src -A$(top_srcdir)/src $(ADAFLAGS) tcprelay

listener: never
	$(GNATMAKE) -m -A../src -A$(top_srcdir)/src $(ADAFLAGS) listener

multi: never
	$(GNATMAKE) -m -A../src -A$(top_srcdir)/src $(ADAFLAGS) multi

DISTCLEANFILES = tcprelay listener multi
CLEANFILES = *.o *.ali b_*.c

never::
