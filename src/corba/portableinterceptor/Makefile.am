SUBDIRS = .

include $(srcdir)/../../Makefile.common

alidir = $(includedir)/polyorb/corba/portableinterceptor

ADA_SPECS_WITH_BODY = \
	polyorb-corba_p-interceptors.ads \
	portableinterceptor.ads \
	portableinterceptor-clientrequestinfo.ads \
	portableinterceptor-clientrequestinfo-impl.ads \
	portableinterceptor-clientrequestinterceptor.ads \
	portableinterceptor-clientrequestinterceptor-impl.ads \
	portableinterceptor-helper.ads \
	portableinterceptor-interceptor.ads \
	portableinterceptor-interceptor-impl.ads \
	portableinterceptor-orbinitializer.ads \
	portableinterceptor-orbinitializer-impl.ads \
	portableinterceptor-orbinitializer-initialize_all.ads \
	portableinterceptor-orbinitializer-register.ads \
	portableinterceptor-orbinitinfo.ads \
	portableinterceptor-orbinitinfo-impl.ads \
	portableinterceptor-requestinfo.ads \
	portableinterceptor-requestinfo-impl.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-corba-portableinterceptor.la
libpolyorb_corba_portableinterceptor_la_SOURCES =
libpolyorb_corba_portableinterceptor_la_LIBADD = $(ADA_LIBADD)
libpolyorb_corba_portableinterceptor_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-corba-portableinterceptor.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. -I$(srcdir)/../.. $(GNATFLAGS)

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

depend-local:
	for d in $(SUBDIRS);do \
		(cd $$d && ${MAKE} depend-local-local); \
	done

depend-local-local: all
	$(srcdir)/../../../support/mkdep -I$(srcdir)/.. -I$(srcdir)/../.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
