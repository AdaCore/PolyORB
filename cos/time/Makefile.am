include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/corba/cos

COS_GNATMAKE_FLAGS = -z -I$(srcdir) \
	-I../event \
	-I$(srcdir)/../event \
	-I$(COS_DIRS) \
	-I../../src \
	-I$(srcdir)/../../src

corba_exe=test_time

PROGS = @APPLI_EXES@

COS_DIRS=../../idls/cos/time

test_time: force
	$(GNATMAKE_FOR_TARGET) $@ $(GNATMAKE_FLAGS)

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	for f in $(srcdir)/cos*.ad[bs] cos*.ali time*.ad[bs] time*.ali; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done
