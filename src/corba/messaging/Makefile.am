SUBDIRS = .

include $(srcdir)/../../Makefile.common

alidir = $(includedir)/polyorb/corba/messaging

ADA_SPECS = \
	messaging.ads

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-corba-messaging.la
libpolyorb_corba_messaging_la_SOURCES =
libpolyorb_corba_messaging_la_LIBADD = $(ADA_LIBADD)
libpolyorb_corba_messaging_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. -I$(srcdir)/../.. $(GNATFLAGS)

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	for f in $(srcdir)/*.ad[bs] *.ali; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
