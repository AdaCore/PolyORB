SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/web_common

ADA_SPECS_WITH_BODY = \
	aws-url.ads \
	aws-url-raise_url_error.ads \
	aws-mime.ads \
	aws-utils.ads \
	md5.ads \
	soap-utils.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	aws.ads \
	soap.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)


ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-web_common.la
libpolyorb_web_common_la_SOURCES =
libpolyorb_web_common_la_LIBADD = $(ADA_LIBADD)
libpolyorb_web_common_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) --mode=link $(ADA) -o libpolyorb-web_common.la

ADAFLAGS = -i -I. -I$(srcdir) -I.. -I$(srcdir)/.. $(GNATFLAGS)

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

depend-local: all
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
aws.lo:aws.ads 
aws-mime.lo:aws.ads aws-mime.adb aws-mime.ads 
aws-url.lo:aws.ads aws-url.adb aws-url.ads aws-url-raise_url_error.ads aws-utils.ads md5.ads ./../polyorb.ads ./../polyorb-utils.adb ./../polyorb-utils.ads 
aws-url-raise_url_error.lo:aws.ads aws-url.ads aws-url-raise_url_error.adb aws-url-raise_url_error.ads 
aws-utils.lo:aws.ads aws-utils.adb aws-utils.ads md5.ads 
md5.lo:md5.adb md5.ads 
soap.lo:soap.ads 
soap-utils.lo:soap.ads soap-utils.adb soap-utils.ads 
