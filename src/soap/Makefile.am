SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(libdir)/polyorb-soap

GPERF_SPECS = \
	polyorb-http_headers.ads \
	polyorb-http_methods.ads
GPERF_C = $(GPERF_SPECS:.ads=_GPERF.c)
GPERF_LIBADD = $(GPERF_C:.c=.lo)

ADA_SPECS_WITH_BODY = \
	$(GPERF_SPECS) \
	polyorb-binding_data-soap.ads \
	polyorb-buffer_sources.ads \
	polyorb-filters-http.ads \
	polyorb-protocols-soap_pr.ads \
	aws-mime.ads \
	aws-response.ads \
	aws-url.ads \
	soap-message-payload.ads \
	soap-message-reader.ads \
	soap-message-response-error.ads \
	soap-message-response.ads \
	soap-message-xml.ads \
	soap-message.ads \
	soap-parameters.ads \
	soap-types.ads \
	soap-utils.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	aws.ads \
	polyorb-filters-aws_interface.ads \
	soap.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-soap.la
libpolyorb_soap_la_SOURCES =
libpolyorb_soap_la_LIBADD = $(ADA_LIBADD) $(GPERF_LIBADD)
libpolyorb_soap_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-soap.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. \
	-I$(srcdir)/../corba $(XMLADA_INCLUDE) $(GNATFLAGS)
# XXX CORBA included temporarily because of dep of P.Binding_Data.SOAP on
# P.Representations.CDR.

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
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. -I$(srcdir)/../corba $(srcdir)
# XXX dep on CORBA is necessary because of P.R.IOR.

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
aws-mime.lo:aws-mime.adb aws-mime.ads aws.ads 
aws-response.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads aws-mime.ads aws-response.adb aws-response.ads aws.ads polyorb-filters-http.ads polyorb-http_methods.ads 
aws-url.lo:aws-url.adb aws-url.ads aws.ads 
aws.lo:aws.ads 
polyorb-binding_data-soap.lo:./../corba/corba-abstractbase.ads ./../corba/corba-context.ads ./../corba/corba-contextlist.ads ./../corba/corba-exceptionlist.ads ./../corba/corba-impl.ads ./../corba/corba-nvlist.ads ./../corba/corba-object.ads ./../corba/corba-request.ads ./../corba/corba.ads ./../corba/polyorb-references-ior.ads ./../corba/polyorb-representations-cdr.ads ./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-configuration.ads ./../polyorb-filters.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb-interface.ads ./../polyorb-orb.ads ./../polyorb-poa_types.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport-sockets.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads aws-url.ads aws.ads polyorb-binding_data-soap.adb polyorb-binding_data-soap.ads polyorb-filters-http.ads polyorb-http_methods.ads polyorb-protocols-soap_pr.ads soap-message-payload.ads soap-message.ads soap-parameters.ads soap.ads 
polyorb-buffer_sources.lo:./../polyorb-buffers.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-sockets.ads ./../polyorb-utils-text_buffers.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-buffer_sources.adb polyorb-buffer_sources.ads 
polyorb-filters-aws_interface.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-filters-interface.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads aws-mime.ads aws-response.ads aws.ads polyorb-filters-aws_interface.ads polyorb-filters-http.ads polyorb-http_methods.ads 
polyorb-filters-http.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-configuration.ads ./../polyorb-filters-interface.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb-interface.ads ./../polyorb-orb.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-text_buffers.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads aws-mime.ads aws-response.ads aws.ads polyorb-filters-aws_interface.ads polyorb-filters-http.adb polyorb-filters-http.ads polyorb-http_headers.ads polyorb-http_methods.ads 
polyorb-http_headers.lo:./../polyorb.ads polyorb-http_headers.adb polyorb-http_headers.ads 
polyorb-http_methods.lo:./../polyorb.ads polyorb-http_methods.adb polyorb-http_methods.ads 
polyorb-protocols-soap_pr.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data-local.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-configuration.ads ./../polyorb-filters-interface.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects-interface.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb-interface.ads ./../polyorb-orb.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads aws-mime.ads aws-response.ads aws.ads polyorb-binding_data-soap.ads polyorb-buffer_sources.ads polyorb-filters-aws_interface.ads polyorb-filters-http.ads polyorb-http_methods.ads polyorb-protocols-soap_pr.adb polyorb-protocols-soap_pr.ads soap-message-payload.ads soap-message-response.ads soap-message-xml.ads soap-message.ads soap-parameters.ads soap.ads 
soap-message-payload.lo:./../polyorb-annotations.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-locks.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads soap-message-payload.adb soap-message-payload.ads soap-message.ads soap-parameters.ads soap.ads 
soap-message-reader.lo:./../polyorb-annotations.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-locks.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads soap-message-reader.adb soap-message-reader.ads soap-message.ads soap-parameters.ads soap.ads 
soap-message-response-error.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads aws-mime.ads aws-response.ads aws.ads polyorb-filters-http.ads polyorb-http_methods.ads soap-message-payload.ads soap-message-response-error.adb soap-message-response-error.ads soap-message-response.ads soap-message.ads soap-parameters.ads soap-types.ads soap-utils.ads soap.ads 
soap-message-response.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads aws-mime.ads aws-response.ads aws.ads polyorb-filters-http.ads polyorb-http_methods.ads soap-message-payload.ads soap-message-response.adb soap-message-response.ads soap-message-xml.ads soap-message.ads soap-parameters.ads soap.ads 
soap-message-xml.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any-objref.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads aws-mime.ads aws-response.ads aws.ads polyorb-binding_data-soap.ads polyorb-filters-http.ads polyorb-http_methods.ads soap-message-payload.ads soap-message-reader.ads soap-message-response-error.ads soap-message-response.ads soap-message-xml.adb soap-message-xml.ads soap-message.ads soap-parameters.ads soap-types.ads soap.ads 
soap-message.lo:./../polyorb-annotations.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-locks.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads soap-message.adb soap-message.ads soap-parameters.ads soap-types.ads soap-utils.ads soap.ads 
soap-parameters.lo:./../polyorb-annotations.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-locks.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads soap-parameters.adb soap-parameters.ads soap-types.ads soap.ads 
soap-types.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any-objref.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-references-binding.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads polyorb-binding_data-soap.ads soap-types.adb soap-types.ads soap-utils.ads soap.ads 
soap-utils.lo:soap-utils.adb soap-utils.ads soap.ads 
soap.lo:soap.ads 
