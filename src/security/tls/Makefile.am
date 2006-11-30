
include $(top_srcdir)/src/Makefile.common

alidir = $(includedir)/polyorb/security

ADA_SPECS_WITH_BODY = \
	polyorb-asynch_ev-sockets-tls.ads \
	polyorb-security-credentials-tls.ads \
	polyorb-security-transport_mechanisms-tls.ads \
	polyorb-tls.ads \
	polyorb-transport-connected-sockets-tls.ads \
	polyorb-utils-tls_access_points.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-security-tls.la

libpolyorb_security_tls_la_SOURCES =
libpolyorb_security_tls_la_LIBADD = $(ADA_LIBADD) \
	$(top_builddir)/src/security/x509/libpolyorb-security-x509.la
libpolyorb_security_tls_la_LDFLAGS = \
	-no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

ADAFLAGS+= $(STD_ADAFLAGS) -I. -I$(srcdir) \
	-I$(top_srcdir)/src -I$(top_srcdir)/src/security \
	-I$(top_srcdir)/src/security/x509

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
