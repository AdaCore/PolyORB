SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(libdir)/polyorb-soap

GPERF_SPECS = \
	polyorb-http_headers.ads \
	polyorb-http_methods.ads
GPERF_BODY = $(GPERF_SPECS:.ads=.adb)

ADA_SPECS_WITH_BODY = \
	$(GPERF_SPECS) \
	polyorb-binding_data-soap.ads \
	polyorb-buffer_sources.ads \
	polyorb-filters-http.ads \
	polyorb-protocols-soap_pr.ads \
	polyorb-setup-access_points-soap.ads \
	polyorb-soap_p-response.ads \
	polyorb-soap_p-message-payload.ads \
	polyorb-soap_p-message-reader.ads \
	polyorb-soap_p-message-response-error.ads \
	polyorb-soap_p-message-response.ads \
	polyorb-soap_p-message-xml.ads \
	polyorb-soap_p-message.ads \
	polyorb-soap_p-parameters.ads \
	polyorb-soap_p-types.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	polyorb-filters-aws_interface.ads \
	polyorb-soap_p.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-soap.la
libpolyorb_soap_la_SOURCES =
libpolyorb_soap_la_LIBADD = $(ADA_LIBADD)
libpolyorb_soap_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali $(GPERF_BODY)

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-soap.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. \
	$(XMLADA_INCS) -I$(srcdir)/../web_common $(GNATFLAGS)

ADACOMPILER = ../../support/adacompiler
CC = $(CC_FOR_TARGET)

GNATPRFH = ../../compilers/gnatprfh/gnatprfh
GEN_HTTP_BODY = $(srcdir)/gen_http_body

SUFFIXES = .adb .ads .lo .ali .c

polyorb-http_methods.adb: $(GEN_HTTP_BODY) $(srcdir)/polyorb-http_methods.ads
	$(GEN_HTTP_BODY) $(GNATPRFH) $(srcdir) Method

polyorb-http_headers.adb: $(GEN_HTTP_BODY) $(srcdir)/polyorb-http_headers.ads
	$(GEN_HTTP_BODY) $(GNATPRFH) $(srcdir) Header

.adb.lo:
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

.ads.lo:
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	for f in $(srcdir)/*.ad[bs] *.ali; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done

depend-local: all
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
