
include $(top_srcdir)/src/Makefile.common
include $(top_srcdir)/src/corba/Makefile.genfiles

alidir = $(includedir)/polyorb/corba/security

ADA_SPECS_WITH_BODY = $(GSSUP_GEN_ADA_SPECS_WITH_BODY)

ADA_SPECS = $(ADA_SPECS_WITH_BODY) $(GSSUP_ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-corba-security-gssup.la

libpolyorb_corba_security_gssup_la_SOURCES =
libpolyorb_corba_security_gssup_la_LIBADD = $(ADA_LIBADD) \
	$(top_builddir)/src/corba/libpolyorb-corba.la \
	$(top_builddir)/src/security/libpolyorb-security.la
libpolyorb_corba_security_gssup_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali $(GSSUP_GEN_ADA) GSSUP.idl-stamp

ADAFLAGS+= $(STD_ADAFLAGS) -I. -I$(srcdir) \
	-I$(top_srcdir)/src -I$(top_srcdir)/src/corba \
	-I$(top_builddir)/src/corba/security \
	-I$(top_srcdir)/src/security

vpath %.idl $(top_srcdir)/idls/Interop

$(GSSUP_GEN_ADA): GSSUP.idl-stamp

IDLAC_FLAGS = -c -I$(top_srcdir)/idls/Interop

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
gssup-helper.lo: $(top_srcdir)/src/corba/corba-sequences-unbounded.ads $(top_srcdir)/src/corba/corba-sequences.ads $(top_srcdir)/src/corba/corba.adb $(top_srcdir)/src/corba/corba.ads $(top_srcdir)/src/corba/polyorb-corba_p-exceptions.ads $(top_srcdir)/src/corba/polyorb-corba_p.ads $(top_builddir)/src/corba/security/csi-helper.ads $(top_builddir)/src/corba/security/csi.ads gssup-helper.adb gssup-helper.ads gssup.ads $(top_srcdir)/src/polyorb-annotations.ads $(top_srcdir)/src/polyorb-any-exceptionlist.ads $(top_srcdir)/src/polyorb-any-nvlist.ads $(top_srcdir)/src/polyorb-any.ads $(top_srcdir)/src/polyorb-asynch_ev.ads $(top_srcdir)/src/polyorb-binding_data.ads $(top_srcdir)/src/polyorb-buffers.ads $(top_srcdir)/src/polyorb-components.ads $(top_srcdir)/src/polyorb-errors-helper.ads $(top_srcdir)/src/polyorb-errors.ads $(top_srcdir)/src/polyorb-exceptions.ads $(top_srcdir)/src/polyorb-initialization.ads $(top_srcdir)/src/polyorb-jobs.ads $(top_srcdir)/src/polyorb-log.adb $(top_srcdir)/src/polyorb-log.ads $(top_srcdir)/src/polyorb-objects.ads $(top_srcdir)/src/polyorb-opaque-chunk_pools.adb $(top_srcdir)/src/polyorb-opaque-chunk_pools.ads $(top_srcdir)/src/polyorb-opaque.ads $(top_srcdir)/src/polyorb-qos.ads $(top_srcdir)/src/polyorb-references.ads $(top_srcdir)/src/polyorb-requests.ads $(top_srcdir)/src/polyorb-sequences-unbounded.adb $(top_srcdir)/src/polyorb-sequences-unbounded.ads $(top_srcdir)/src/polyorb-sequences.ads $(top_srcdir)/src/polyorb-smart_pointers.ads $(top_srcdir)/src/polyorb-task_info.ads $(top_srcdir)/src/polyorb-tasking-condition_variables.ads $(top_srcdir)/src/polyorb-tasking-mutexes.ads $(top_srcdir)/src/polyorb-tasking-threads.ads $(top_srcdir)/src/polyorb-tasking.ads $(top_srcdir)/src/polyorb-transport.ads $(top_srcdir)/src/polyorb-types.adb $(top_srcdir)/src/polyorb-types.ads $(top_srcdir)/src/polyorb-utils-chained_lists.adb $(top_srcdir)/src/polyorb-utils-chained_lists.ads $(top_srcdir)/src/polyorb-utils-simple_flags.adb $(top_srcdir)/src/polyorb-utils-simple_flags.ads $(top_srcdir)/src/polyorb-utils-strings-lists.adb $(top_srcdir)/src/polyorb-utils-strings-lists.ads $(top_srcdir)/src/polyorb-utils-strings.adb $(top_srcdir)/src/polyorb-utils-strings.ads $(top_srcdir)/src/polyorb-utils.ads $(top_srcdir)/src/polyorb.ads
