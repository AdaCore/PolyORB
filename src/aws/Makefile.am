SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(libdir)/polyorb-aws

ADA_SPECS_WITH_BODY = \
	aws-server.ads \
	aws-server-get_status.ads \
	aws-client.ads \
	aws-response.ads \
	aws-status.ads \
	aws-response-set.ads \
	aws-hotplug.ads \
	aws-hotplug-get_status.ads \
	aws-utils.ads \
	aws-dispatchers.ads \
	aws-dispatchers-callback.ads \
	aws-config.ads \
	aws-config-set.ads \
	aws-config-ini.ads \
	aws-session-control.ads \
	aws-parameters.ads \
	aws-parameters-set.ads \
	aws-session.ads \
	aws-url.ads \
	aws-url-raise_url_error.ads \
	aws-headers.ads \
	aws-headers-values.ads \
	aws-headers-set.ads \
	aws-translator.ads \
	aws-messages.ads \
	aws-resources.ads \
	aws-resources-streams.ads \
	aws-resources-files.ads \
	aws-resources-embedded.ads \
	aws-mime.ads \
	aws-log.ads \
	aws-digest.ads \
	aws-os_lib.ads \
	aws-containers-tables-set.ads \
	include/md5.ads \
	include/templates_parser-input.ads \
	include/table_of_strings_and_static_values_g.ads \
	include/table_of_dynamic_keys_and_static_values_g.ads \
	include/table_of_static_keys_and_dynamic_values_g.ads \
	include/templates_parser.ads


ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	aws-default.ads \
	aws-exceptions.ads \
	aws-containers.ads \
	aws-templates.ads \
	aws-containers-key_value.ads


ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)	

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-aws.la
libpolyorb_aws_la_SOURCES =
libpolyorb_aws_la_LIBADD = $(ADA_LIBADD)
libpolyorb_aws_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-aws.la

ADAFLAGS = -i -I. -I$(srcdir) -I.. -I$(srcdir)/.. $(GNATFLAGS) -I./include/

ADACOMPILER = ../../support/adacompiler

SUFFIXES = .adb .ads .lo .ali

.adb.lo:
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

.ads.lo:
	@echo "youpi!!!"
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	for f in $(srcdir)/*.ad[bs] *.ali; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done

depend-local: all
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###





