
SUBDIRS = . gssup

include $(top_srcdir)/src/Makefile.common
include $(top_srcdir)/src/corba/Makefile.genfiles

alidir = $(includedir)/polyorb/corba/security

ADA_SPECS_WITH_BODY = \
	$(CSI_GEN_ADA_SPECS_WITH_BODY) \
	$(CSIIOP_GEN_ADA_SPECS_WITH_BODY) \
	polyorb-corba_p-css_state_machine.ads \
	polyorb-corba_p-css_state_machine_actions.ads \
	polyorb-corba_p-security_current.ads \
	polyorb-corba_p-security_policy.ads \
	polyorb-corba_p-tss_state_machine.ads \
	polyorb-corba_p-tss_state_machine_actions.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) $(CSI_GEN_ADA_SPECS) $(CSIIOP_GEN_ADA_SPECS)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-corba-security.la

libpolyorb_corba_security_la_SOURCES =
libpolyorb_corba_security_la_LIBADD = $(ADA_LIBADD) \
	$(top_builddir)/src/corba/libpolyorb-corba.la \
	$(top_builddir)/src/security/libpolyorb-security.la
libpolyorb_corba_security_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali $(CSI_GEN_ADA) $(CSIIOP_GEN_ADA) *.idl-stamp

ADAFLAGS+= $(STD_ADAFLAGS) -I. -I$(srcdir) \
	-I$(top_srcdir)/src -I$(top_srcdir)/src/corba \
	-I$(top_builddir)/src/corba/iop \
	-I$(top_srcdir)/src/security

vpath %.idl $(top_srcdir)/idls/Interop

$(CSI_GEN_ADA): CSI.idl-stamp
$(CSIIOP_GEN_ADA): CSIIOP.idl-stamp

IDLAC_FLAGS = -c -I$(top_srcdir)/idls/Interop -I$(top_srcdir)/idls/CORBA_IDL \
	-I$(top_srcdir)/idls/CORBA_PIDL

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
