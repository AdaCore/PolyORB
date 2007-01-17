
include $(top_srcdir)/src/Makefile.common

alidir = $(includedir)/polyorb/security

ADA_SPECS_WITH_BODY = \
	polyorb-security-identities-distinguished_name.ads \
	polyorb-x509.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-security-x509.la

libpolyorb_security_x509_la_SOURCES = polyorb_x509.c
libpolyorb_security_x509_la_LIBADD = $(ADA_LIBADD) \
	$(top_builddir)/src/security/libpolyorb-security.la
libpolyorb_security_x509_la_LDFLAGS = \
	-no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

ADAFLAGS+= $(STD_ADAFLAGS) -I. -I$(srcdir) \
	-I$(top_srcdir)/src -I$(top_srcdir)/src/security

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb-security-identities-distinguished_name.lo: $(top_srcdir)/src/polyorb-errors.ads $(top_srcdir)/src/polyorb-initialization.ads $(top_srcdir)/src/polyorb-log.adb $(top_srcdir)/src/polyorb-log.ads $(top_srcdir)/src/polyorb-smart_pointers.ads $(top_srcdir)/src/polyorb-tasking-mutexes.ads $(top_srcdir)/src/polyorb-tasking.ads $(top_srcdir)/src/polyorb-types.ads $(top_srcdir)/src/polyorb-utils-chained_lists.adb $(top_srcdir)/src/polyorb-utils-chained_lists.ads $(top_srcdir)/src/polyorb-utils-simple_flags.adb $(top_srcdir)/src/polyorb-utils-simple_flags.ads $(top_srcdir)/src/polyorb-utils-strings-lists.adb $(top_srcdir)/src/polyorb-utils-strings-lists.ads $(top_srcdir)/src/polyorb-utils-strings.adb $(top_srcdir)/src/polyorb-utils-strings.ads $(top_srcdir)/src/polyorb-utils.ads $(top_srcdir)/src/polyorb.ads $(top_srcdir)/src/security/polyorb-asn1.ads $(top_srcdir)/src/security/polyorb-security-identities.ads $(top_srcdir)/src/security/polyorb-security-types.ads $(top_srcdir)/src/security/polyorb-security.ads $(top_srcdir)/src/security/x509/polyorb-security-identities-distinguished_name.adb $(top_srcdir)/src/security/x509/polyorb-security-identities-distinguished_name.ads $(top_srcdir)/src/security/x509/polyorb-x509.ads
polyorb-x509.lo: $(top_srcdir)/src/polyorb-initialization.ads $(top_srcdir)/src/polyorb-tasking-mutexes.ads $(top_srcdir)/src/polyorb-tasking-threads.adb $(top_srcdir)/src/polyorb-tasking-threads.ads $(top_srcdir)/src/polyorb-tasking.ads $(top_srcdir)/src/polyorb-utils-chained_lists.adb $(top_srcdir)/src/polyorb-utils-chained_lists.ads $(top_srcdir)/src/polyorb-utils-strings-lists.adb $(top_srcdir)/src/polyorb-utils-strings-lists.ads $(top_srcdir)/src/polyorb-utils-strings.adb $(top_srcdir)/src/polyorb-utils-strings.ads $(top_srcdir)/src/polyorb-utils.ads $(top_srcdir)/src/polyorb.ads $(top_srcdir)/src/security/x509/polyorb-x509.adb $(top_srcdir)/src/security/x509/polyorb-x509.ads
