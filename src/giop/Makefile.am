SUBDIRS = . iiop diop miop

include $(srcdir)/../Makefile.common

alidir = $(libdir)/polyorb-giop

ADA_SPECS_WITH_BODY = \
	polyorb-giop_p-exceptions.ads \
	polyorb-giop_p-tagged_components.ads \
	polyorb-protocols-giop-common.ads \
	polyorb-protocols-giop-giop_1_0.ads \
	polyorb-protocols-giop-giop_1_1.ads \
	polyorb-protocols-giop-giop_1_2.ads \
	polyorb-protocols-giop.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	polyorb-giop_p.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-giop.la
libpolyorb_giop_la_SOURCES =
libpolyorb_giop_la_LIBADD = $(ADA_LIBADD)
libpolyorb_giop_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-giop.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. $(GNATFLAGS)

ADACOMPILER = ../../support/adacompiler

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

depend-local:
	for d in $(SUBDIRS);do \
		(cd $$d && ${MAKE} depend-local-local); \
	done

depend-local-local: all
	$(srcdir)/../../support/mkdep $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb-giop_p.lo:polyorb-giop_p.ads 
polyorb-giop_p-exceptions.lo:polyorb-giop_p.ads polyorb-giop_p-exceptions.adb polyorb-giop_p-exceptions.ads 
polyorb-giop_p-tagged_components.lo:polyorb-giop_p.ads polyorb-giop_p-tagged_components.adb polyorb-giop_p-tagged_components.ads 
polyorb-protocols-giop.lo:polyorb-giop_p.ads polyorb-giop_p-exceptions.ads polyorb-protocols-giop.adb polyorb-protocols-giop.ads 
polyorb-protocols-giop-common.lo:polyorb-giop_p.ads polyorb-giop_p-exceptions.ads polyorb-protocols-giop.ads polyorb-protocols-giop-common.adb polyorb-protocols-giop-common.ads 
polyorb-protocols-giop-giop_1_0.lo:polyorb-giop_p.ads polyorb-giop_p-exceptions.ads polyorb-protocols-giop.ads polyorb-protocols-giop-common.adb polyorb-protocols-giop-common.ads polyorb-protocols-giop-giop_1_0.adb polyorb-protocols-giop-giop_1_0.ads 
polyorb-protocols-giop-giop_1_1.lo:polyorb-giop_p.ads polyorb-giop_p-exceptions.ads polyorb-protocols-giop.ads polyorb-protocols-giop-common.adb polyorb-protocols-giop-common.ads polyorb-protocols-giop-giop_1_1.adb polyorb-protocols-giop-giop_1_1.ads 
polyorb-protocols-giop-giop_1_2.lo:polyorb-giop_p.ads polyorb-giop_p-exceptions.ads polyorb-protocols-giop.ads polyorb-protocols-giop-common.adb polyorb-protocols-giop-common.ads polyorb-protocols-giop-giop_1_2.adb polyorb-protocols-giop-giop_1_2.ads 
