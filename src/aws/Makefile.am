SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/aws

ADA_SPECS_WITH_BODY = \
	aws-server.ads \
	aws-server-servants.ads \
	aws-server-get_status.ads \
	aws-client.ads \
	aws-status.ads \
	aws-status-set.ads \
	aws-status-translate_table.ads \
	aws-headers.ads \
	aws-headers-values.ads \
	aws-headers-set.ads \
	aws-response.ads	 \
	aws-response-set.ads \
	aws-parameters-set.ads \
	soap-types.ads \
	soap-message.ads \
	soap-message-payload.ads \
	soap-message-response.ads \
	soap-message-response-error.ads \
	soap-parameters.ads \
	soap-client.ads


ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	aws-exceptions.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)


ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-aws.la
libpolyorb_aws_la_SOURCES =
libpolyorb_aws_la_LIBADD = $(ADA_LIBADD)
libpolyorb_aws_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-aws.la

ADAFLAGS = -i -I. -I$(srcdir) -I.. -I$(srcdir)/.. -I$(srcdir)/../web_common -I$(srcdir)/../aws_orig $(GNATFLAGS)

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
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. $(srcdir) -I$(srcdir)/../web_common -I$(srcdir)/../aws_orig

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###





