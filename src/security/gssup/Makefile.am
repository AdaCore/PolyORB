
include $(top_srcdir)/src/Makefile.common

alidir = $(includedir)/polyorb/security

ADA_SPECS_WITH_BODY = \
	polyorb-security-authentication_mechanisms-gssup_client.ads \
	polyorb-security-authentication_mechanisms-gssup_target.ads \
	polyorb-security-credentials-gssup.ads \
	polyorb-security-exported_names-gssup.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-security-gssup.la

libpolyorb_security_gssup_la_SOURCES =
libpolyorb_security_gssup_la_LIBADD = $(ADA_LIBADD) \
	$(top_builddir)/src/security/libpolyorb-security.la
libpolyorb_security_gssup_la_LDFLAGS = \
	-no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

ADAFLAGS+= $(STD_ADAFLAGS) -I. -I$(srcdir) \
	-I$(top_srcdir)/src -I$(top_srcdir)/src/security

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
