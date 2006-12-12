SUBDIRS = .

include $(srcdir)/../../Makefile.common
include $(srcdir)/../Makefile.genfiles

alidir = $(includedir)/polyorb/corba/messaging

ADA_SPECS_WITH_BODY = \
	$(MESSAGING_GEN_ADA_SPECS_WITH_BODY) 

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	$(MESSAGING_GEN_ADA_SPECS)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-corba-messaging.la

libpolyorb_corba_messaging_la_SOURCES =
libpolyorb_corba_messaging_la_LIBADD = $(ADA_LIBADD)
libpolyorb_corba_messaging_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali $(MESSAGING_GEN_ADA) MESSAGING.idl-stamp

ADAFLAGS+= -I. -I$(srcdir) -I.. -I$(srcdir)/.. -I$(srcdir)/../.. $(STD_ADAFLAGS)

$(MESSAGING_GEN_ADA): MESSAGING.idl-stamp

MESSAGING.idl-stamp: $(top_srcdir)/idls/Misc/Messaging.idl $(IDLAC_bin)
	$(IDLAC_WRAPPER) --idlac=$(IDLAC_bin) -I$(top_srcdir)/idls/CORBA_IDL -I$(top_srcdir)/idls/CORBA_PIDL $(top_srcdir)/idls/Misc/Messaging.idl
	touch MESSAGING.idl-stamp

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
messaging-helper.lo: $(top_srcdir)/src/corba/corba-helper.ads $(top_srcdir)/src/corba/corba-sequences-unbounded.ads $(top_srcdir)/src/corba/corba-sequences.ads $(top_srcdir)/src/corba/corba.adb $(top_srcdir)/src/corba/corba.ads messaging-helper.adb messaging-helper.ads messaging.ads $(top_srcdir)/src/corba/polyorb-corba_p-exceptions.ads $(top_srcdir)/src/corba/polyorb-corba_p.ads $(top_srcdir)/src/corba/polyorb-sequences-unbounded-corba_helper.adb $(top_srcdir)/src/corba/polyorb-sequences-unbounded-corba_helper.ads $(top_srcdir)/src/polyorb-annotations.ads $(top_srcdir)/src/polyorb-any-exceptionlist.ads $(top_srcdir)/src/polyorb-any-nvlist.ads $(top_srcdir)/src/polyorb-any.ads $(top_srcdir)/src/polyorb-asynch_ev.ads $(top_srcdir)/src/polyorb-binding_data.ads $(top_srcdir)/src/polyorb-buffers.ads $(top_srcdir)/src/polyorb-components.ads $(top_srcdir)/src/polyorb-errors-helper.ads $(top_srcdir)/src/polyorb-errors.ads $(top_srcdir)/src/polyorb-exceptions.ads $(top_srcdir)/src/polyorb-initialization.ads $(top_srcdir)/src/polyorb-jobs.ads $(top_srcdir)/src/polyorb-log.adb $(top_srcdir)/src/polyorb-log.ads $(top_srcdir)/src/polyorb-objects.ads $(top_srcdir)/src/polyorb-opaque-chunk_pools.adb $(top_srcdir)/src/polyorb-opaque-chunk_pools.ads $(top_srcdir)/src/polyorb-opaque.ads $(top_srcdir)/src/polyorb-qos.ads $(top_srcdir)/src/polyorb-references.ads $(top_srcdir)/src/polyorb-requests.ads $(top_srcdir)/src/polyorb-sequences-helper.adb $(top_srcdir)/src/polyorb-sequences-helper.ads $(top_srcdir)/src/polyorb-sequences-unbounded-helper.adb $(top_srcdir)/src/polyorb-sequences-unbounded-helper.ads $(top_srcdir)/src/polyorb-sequences-unbounded.adb $(top_srcdir)/src/polyorb-sequences-unbounded.ads $(top_srcdir)/src/polyorb-sequences.ads $(top_srcdir)/src/polyorb-smart_pointers.ads $(top_srcdir)/src/polyorb-task_info.ads $(top_srcdir)/src/polyorb-tasking-condition_variables.ads $(top_srcdir)/src/polyorb-tasking-mutexes.ads $(top_srcdir)/src/polyorb-tasking-threads.ads $(top_srcdir)/src/polyorb-tasking.ads $(top_srcdir)/src/polyorb-transport.ads $(top_srcdir)/src/polyorb-types.adb $(top_srcdir)/src/polyorb-types.ads $(top_srcdir)/src/polyorb-utils-chained_lists.adb $(top_srcdir)/src/polyorb-utils-chained_lists.ads $(top_srcdir)/src/polyorb-utils-simple_flags.adb $(top_srcdir)/src/polyorb-utils-simple_flags.ads $(top_srcdir)/src/polyorb-utils-strings-lists.adb $(top_srcdir)/src/polyorb-utils-strings-lists.ads $(top_srcdir)/src/polyorb-utils-strings.adb $(top_srcdir)/src/polyorb-utils-strings.ads $(top_srcdir)/src/polyorb-utils.ads $(top_srcdir)/src/polyorb.ads
messaging.lo: $(top_srcdir)/src/corba/corba-sequences-unbounded.ads $(top_srcdir)/src/corba/corba-sequences.ads $(top_srcdir)/src/corba/corba.ads messaging.ads $(top_srcdir)/src/polyorb-any.ads $(top_srcdir)/src/polyorb-errors.ads $(top_srcdir)/src/polyorb-sequences-unbounded.adb $(top_srcdir)/src/polyorb-sequences-unbounded.ads $(top_srcdir)/src/polyorb-sequences.adb $(top_srcdir)/src/polyorb-sequences.ads $(top_srcdir)/src/polyorb-smart_pointers.ads $(top_srcdir)/src/polyorb-tasking-mutexes.ads $(top_srcdir)/src/polyorb-tasking.ads $(top_srcdir)/src/polyorb-types.ads $(top_srcdir)/src/polyorb.ads
