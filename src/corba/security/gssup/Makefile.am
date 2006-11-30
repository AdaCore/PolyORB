
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
