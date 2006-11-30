
include $(top_srcdir)/src/Makefile.common

SUBDIRS = . tls

alidir = $(includedir)/polyorb/giop/iiop/security

ADA_SPECS_WITH_BODY = \
	polyorb-giop_p-tagged_components-csi_sec_mech_list.ads \
	polyorb-giop_p-tagged_components-null_tag.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-giop-iiop-security.la

libpolyorb_giop_iiop_security_la_SOURCES =
libpolyorb_giop_iiop_security_la_LIBADD = $(ADA_LIBADD) \
	$(top_builddir)/src/giop/iiop/libpolyorb-giop-iiop.la \
	$(top_builddir)/src/security/libpolyorb-security.la
libpolyorb_giop_iiop_security_la_LDFLAGS = \
	-no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

ADAFLAGS+= $(STD_ADAFLAGS) -I. -I$(srcdir) \
	-I$(top_srcdir)/src -I$(top_srcdir)/src/giop \
	-I$(top_srcdir)/src/giop/iiop -I$(top_srcdir)/src/security

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
