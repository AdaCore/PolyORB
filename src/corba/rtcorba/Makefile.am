iiSUBDIRS = .

include $(srcdir)/../../Makefile.common

alidir = $(includedir)/polyorb/corba/rtcorba

ADA_SPECS_WITH_BODY = \
		polyorb-rtcorba_p-prioritymodelpolicy.ads \
		polyorb-rtcorba_p-threadpoolmanager.ads \
		polyorb-rtcorba_p-to_orb_priority.ads \
		polyorb-rtcorba_p-setup.ads \
		rtcorba.ads \
		rtcorba-current.ads \
		rtcorba-mutex.ads \
		rtcorba-rtorb.ads \
		rtcorba-prioritymapping.ads \
		rtcorba-prioritymapping-linear.ads \
		rtcorba-prioritymodelpolicy.ads \
		rtcorba-prioritytransform.ads \
		rtcorba-threadpoolpolicy.ads \
		rtportableserver-poa.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
		polyorb-rtcorba_p.ads \
		rtportableserver.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-corba-rtcorba.la
libpolyorb_corba_rtcorba_la_SOURCES =
libpolyorb_corba_rtcorba_la_LIBADD = $(ADA_LIBADD)
libpolyorb_corba_rtcorba_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) --mode=link $(ADA) -o libpolyorb-corba-rtcorba.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. -I../../ -I$(srcdir)/../.. $(GNATFLAGS)

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
	$(srcdir)/../../../support/mkdep -I$(srcdir)/.. -I$(srcdir)/../.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
