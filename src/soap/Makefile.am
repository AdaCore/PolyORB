SUBDIRS = .

alidir = $(libdir)/polyorb-soap

GPERF_SPECS = \
	polyorb-http_headers.ads \
	polyorb-http_methods.ads
GPERF_C = $(GPERF_SPECS:.ads=_GPERF.c)
GPERF_LIBADD = $(GPERF_C:.c=.lo)

ADA_SPECS_WITH_BODY = \
	$(GPERF_SPECS) \
	polyorb-filters-http.ads \
	polyorb-protocols-soap.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-soap.la
libpolyorb_soap_la_SOURCES =
libpolyorb_soap_la_LIBADD = $(ADA_LIBADD) $(GPERF_LIBADD)
libpolyorb_soap_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-soap.la

ADAFLAGS = $(GNATFLAGS) -I. -I$(srcdir) -I.. -I$(srcdir)/..

ADACOMPILER = ../../support/adacompiler

SUFFIXES = .adb .ads .lo .ali .c

polyorb-http_methods_GPERF.c polyorb-http_methods.adb: \
  polyorb-http_methods.ads
	$(srcdir)/gen_keywords $(srcdir)/polyorb-http_methods.ads

polyorb-http_headers_GPERF.c polyorb-http_headers.adb: \
  polyorb-http_headers.ads
	$(srcdir)/gen_keywords $(srcdir)/polyorb-http_headers.ads

.c.lo:
	$(LTCOMPILE) -c $<

.adb.lo:
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

.ads.lo:
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 $(srcdir)/*.ad[bs] $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 *.ali $(DESTDIR)$(alidir)

depend-local: all
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
