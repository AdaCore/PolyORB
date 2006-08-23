SUBDIRS = .

include $(srcdir)/../../Makefile.common
include $(srcdir)/../Makefile.genfiles

alidir = $(includedir)/polyorb/corba/dynamicany

ADA_SPECS_WITH_BODY = \
	$(DYNAMICANY_GEN_ADA_SPECS_WITH_BODY) 
	dynamicany-dynany-impl.ads \
	dynamicany-dynanyfactory-impl.ads \
	dynamicany-dynarray-impl.ads \
	dynamicany-dynenum-impl.ads \
	dynamicany-dynfixed-impl.ads \
	dynamicany-dynsequence-impl.ads \
	dynamicany-dynstruct-impl.ads \
	dynamicany-dynunion-impl.ads \
	dynamicany-dynvalue-impl.ads \
	dynamicany-dynvaluebox-impl.ads \
	dynamicany-dynvaluecommon-impl.ads \
	polyorb-corba_p-dynamic_any.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	$(DYNAMICANY_GEN_ADA_SPECS)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-corba-dynamicany.la
libpolyorb_corba_dynamicany_la_SOURCES =
libpolyorb_corba_dynamicany_la_LIBADD = $(ADA_LIBADD)
libpolyorb_corba_dynamicany_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali $(DYNAMICANY_GEN_ADA) DYNAMICANY.idl-stamp

ADAFLAGS+= -I. -I$(srcdir) -I.. -I$(srcdir)/.. -I$(srcdir)/../.. $(STD_ADAFLAGS)
$(DYNAMICANY_GEN_ADA): DYNAMICANY.idl-stamp

DYNAMICANY.idl-stamp: $(top_srcdir)/idls/Misc/DynamicAny.idl $(IDLAC_bin)
	$(IDLAC_WRAPPER) --idlac=$(IDLAC_bin) -I$(top_srcdir)/idls/CORBA_IDL -I$(top_srcdir)/idls/CORBA_PIDL $(top_srcdir)/idls/Misc/DynamicAny.idl
	touch DYNAMICANY.idl-stamp

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	for f in $(srcdir)/*.ad[bs] *.ali; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
