
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
