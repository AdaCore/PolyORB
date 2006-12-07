
include $(top_srcdir)/src/Makefile.common

alidir = $(includedir)/polyorb/giop/iiop/security/tls

ADA_SPECS_WITH_BODY = \
	polyorb-giop_p-tagged_components-tls_sec_trans.ads \
	polyorb-giop_p-transport_mechanisms-tls.ads \
	polyorb-setup-access_points-tlsiop.ads \
	polyorb-setup-tlsiop.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-giop-iiop-security-tls.la

libpolyorb_giop_iiop_security_tls_la_SOURCES =
libpolyorb_giop_iiop_security_tls_la_LIBADD = $(ADA_LIBADD) \
	$(top_builddir)/src/security/tls/libpolyorb-security-tls.la \
	$(top_builddir)/src/giop/iiop/security/libpolyorb-giop-iiop-security.la
libpolyorb_giop_iiop_security_tls_la_LDFLAGS = \
	-no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

ADAFLAGS+= $(STD_ADAFLAGS) -I. -I$(srcdir) \
	-I$(top_srcdir)/src -I$(top_srcdir)/src/giop \
	-I$(top_srcdir)/src/giop/iiop -I$(top_srcdir)/src/security \
	-I$(top_srcdir)/src/security/x509 -I$(top_srcdir)/src/security/tls \
	-I$(top_srcdir)/src/giop/iiop/security

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
