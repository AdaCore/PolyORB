SUBDIRS = .

include $(srcdir)/../../Makefile.common

alidir = $(libdir)/polyorb-iiop

ADA_SPECS_WITH_BODY = \
	polyorb-binding_data-iiop.ads \
	polyorb-setup-giop.ads \
	polyorb-setup-tcp_access_points-iiop.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) 

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-iiop.la
libpolyorb_iiop_la_SOURCES =
libpolyorb_iiop_la_LIBADD = $(ADA_LIBADD)
libpolyorb_iiop_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-iiop.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I ../../ -I$(srcdir)/.. -I$(srcdir)/../.. $(GNATFLAGS)

ADACOMPILER = ../../../support/adacompiler

SUFFIXES = .adb .ads .lo .ali

.adb.lo:
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

.ads.lo:
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	for f in $(srcdir)/*.ad[bs] *.ali; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done

depend-local-local: all
	$(srcdir)/../../../support/mkdep -I$(srcdir)/.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb-binding_data-iiop.lo:polyorb-binding_data-iiop.adb polyorb-binding_data-iiop.ads ./../polyorb-giop_p.ads ./../polyorb-giop_p-tagged_components.ads ./../polyorb-protocols-giop.ads 
polyorb-setup-giop.lo:./../polyorb-protocols-giop.ads ./../polyorb-protocols-giop-common.ads ./../polyorb-protocols-giop-giop_1_0.ads ./../polyorb-protocols-giop-giop_1_1.ads ./../polyorb-protocols-giop-giop_1_2.ads polyorb-setup-giop.adb polyorb-setup-giop.ads 
polyorb-setup-tcp_access_points-iiop.lo:polyorb-binding_data-iiop.ads ./../polyorb-giop_p.ads ./../polyorb-giop_p-tagged_components.ads ./../polyorb-protocols-giop.ads polyorb-setup-tcp_access_points-iiop.adb polyorb-setup-tcp_access_points-iiop.ads 
