SUBDIRS = .

alidir = $(libdir)/polyorb-soap

ADA_SPECS_WITH_BODY = \
	polyorb-binding_data-soap.ads \
	polyorb-filters-http.ads \
	polyorb-protocols-http.ads \
	polyorb-protocols-soap.ads \
	polyorb-representations-http.ads \
	polyorb-representations-soap-any.ads \
	polyorb-representations-soap-name_spaces.ads \
	polyorb-representations-soap.ads \
	polyorb-utils-http.ads \
	polyorb-utils-http_messages.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-soap.la
libpolyorb_soap_la_SOURCES =
libpolyorb_soap_la_LIBADD = $(ADA_LIBADD)
libpolyorb_soap_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-soap.la

ADAFLAGS = $(GNATFLAGS) \
	-I. -I$(srcdir) -I.. -I$(srcdir)/..

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
	$(INSTALL) -m 444 $(srcdir)/*.ad[bs] $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 *.ali $(DESTDIR)$(alidir)

depend-local: all
	$(srcdir)/../../support/mkdep $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb-binding_data-soap.lo:polyorb-binding_data-soap.adb polyorb-binding_data-soap.ads polyorb-filters-http.ads polyorb-protocols-http.ads polyorb-protocols-soap.ads polyorb-representations-soap.ads polyorb-utils-http.ads polyorb-utils-http_messages.ads 
polyorb-filters-http.lo:polyorb-filters-http.adb polyorb-filters-http.ads 
polyorb-protocols-http.lo:polyorb-protocols-http.adb polyorb-protocols-http.ads polyorb-utils-http.ads polyorb-utils-http_messages.ads 
polyorb-protocols-soap.lo:polyorb-binding_data-soap.ads polyorb-protocols-http.ads polyorb-protocols-soap.adb polyorb-protocols-soap.ads polyorb-representations-soap-any.ads polyorb-representations-soap.ads polyorb-utils-http.ads polyorb-utils-http_messages.ads 
polyorb-representations-http.lo:polyorb-representations-http.adb polyorb-representations-http.ads 
polyorb-representations-soap-any.lo:polyorb-representations-soap-any.adb polyorb-representations-soap-any.ads polyorb-representations-soap.ads 
polyorb-representations-soap-name_spaces.lo:polyorb-protocols-http.ads polyorb-protocols-soap.ads polyorb-representations-soap-name_spaces.adb polyorb-representations-soap-name_spaces.ads polyorb-representations-soap.ads polyorb-utils-http_messages.ads 
polyorb-representations-soap.lo:polyorb-representations-soap.adb polyorb-representations-soap.ads 
polyorb-utils-http.lo:polyorb-utils-http.adb polyorb-utils-http.ads 
polyorb-utils-http_messages.lo:polyorb-protocols-http.ads polyorb-utils-http_messages.adb polyorb-utils-http_messages.ads 
