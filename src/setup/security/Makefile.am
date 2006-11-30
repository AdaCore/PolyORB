
include $(top_srcdir)/src/Makefile.common

alidir = $(includedir)/polyorb/security

ADA_SPECS_WITH_BODY = \
	polyorb-setup-secure_client.ads \
	polyorb-setup-security_base.ads \
	polyorb-setup-secure_server.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-setup-security.la

libpolyorb_setup_security_la_SOURCES =
libpolyorb_setup_security_la_LIBADD = $(ADA_LIBADD) \
	$(top_builddir)/src/corba/security/libpolyorb-corba-security.la \
	$(top_builddir)/src/security/gssup/libpolyorb-security-gssup.la \
	$(top_builddir)/src/giop/iiop/security/libpolyorb-giop-iiop-security.la \
	$(top_builddir)/src/giop/iiop/security/tls/libpolyorb-giop-iiop-security-tls.la
libpolyorb_setup_security_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

ADAFLAGS+= $(STD_ADAFLAGS) -I. -I$(srcdir) \
	-I$(top_srcdir)/src \
	-I$(top_srcdir)/src/corba \
	-I$(top_srcdir)/src/giop \
	-I$(top_srcdir)/src/security \
	-I$(top_srcdir)/src/security/gssup \
	-I$(top_srcdir)/src/corba/security \
	-I$(top_srcdir)/src/giop/iiop/security \
	-I$(top_srcdir)/src/giop/iiop/security/tls

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
