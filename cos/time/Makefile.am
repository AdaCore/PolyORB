include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/corba/cos

COS_GNATMAKE_FLAGS = -z -I$(srcdir) \
	-I../event \
	-I$(srcdir)/../event \
	-I$(COS_DIRS) \
	-I../../src \
	-I$(srcdir)/../../src

corba_exe=

PROGS = @APPLI_EXES@

COS_DIRS=../../idls/cos/time

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	for f in $(srcdir)/cos*.ad[bs] $(srcdir)/time*.ad[bs]; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done
