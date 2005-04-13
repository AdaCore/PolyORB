SUBDIRS = .

include $(srcdir)/../../Makefile.common

alidir = $(includedir)/polyorb/corba/messaging

ADA_SPECS_WITH_BODY = \
	messaging-helper.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
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
messaging.lo: $(top_srcdir)/src/corba/corba-sequences-unbounded.ads $(top_srcdir)/src/corba/corba-sequences.ads $(top_srcdir)/src/corba/corba.ads $(top_srcdir)/src/corba/messaging/messaging.ads $(top_srcdir)/src/polyorb-any.ads $(top_srcdir)/src/polyorb-errors.ads $(top_srcdir)/src/polyorb-sequences-unbounded.adb $(top_srcdir)/src/polyorb-sequences-unbounded.ads $(top_srcdir)/src/polyorb-sequences.ads $(top_srcdir)/src/polyorb-smart_pointers.ads $(top_srcdir)/src/polyorb-tasking-mutexes.ads $(top_srcdir)/src/polyorb-tasking.ads $(top_srcdir)/src/polyorb-types.ads $(top_srcdir)/src/polyorb.ads
messaging-helper.lo: $(top_srcdir)/src/corba/corba-helper.ads $(top_srcdir)/src/corba/corba-sequences-unbounded.ads $(top_srcdir)/src/corba/corba-sequences.ads $(top_srcdir)/src/corba/corba.adb $(top_srcdir)/src/corba/corba.ads $(top_srcdir)/src/corba/messaging/messaging-helper.adb $(top_srcdir)/src/corba/messaging/messaging-helper.ads $(top_srcdir)/src/corba/messaging/messaging.ads $(top_srcdir)/src/corba/polyorb-corba_p-exceptions.ads $(top_srcdir)/src/corba/polyorb-corba_p.ads $(top_srcdir)/src/corba/polyorb-sequences-unbounded-corba_helper.adb $(top_srcdir)/src/corba/polyorb-sequences-unbounded-corba_helper.ads $(top_srcdir)/src/polyorb-any.ads $(top_srcdir)/src/polyorb-errors-helper.ads $(top_srcdir)/src/polyorb-errors.ads $(top_srcdir)/src/polyorb-exceptions.ads $(top_srcdir)/src/polyorb-initialization.ads $(top_srcdir)/src/polyorb-sequences-unbounded.adb $(top_srcdir)/src/polyorb-sequences-unbounded.ads $(top_srcdir)/src/polyorb-sequences.ads $(top_srcdir)/src/polyorb-smart_pointers.ads $(top_srcdir)/src/polyorb-tasking-mutexes.ads $(top_srcdir)/src/polyorb-tasking.ads $(top_srcdir)/src/polyorb-types.adb $(top_srcdir)/src/polyorb-types.ads $(top_srcdir)/src/polyorb-utils-chained_lists.adb $(top_srcdir)/src/polyorb-utils-chained_lists.ads $(top_srcdir)/src/polyorb-utils-strings-lists.adb $(top_srcdir)/src/polyorb-utils-strings-lists.ads $(top_srcdir)/src/polyorb-utils-strings.adb $(top_srcdir)/src/polyorb-utils-strings.ads $(top_srcdir)/src/polyorb-utils.ads $(top_srcdir)/src/polyorb.ads
