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
	polyorb-setup-tcp_access_points-soap.ads \
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
libpolyorb_soap_la_LIBADD = $(ADA_LIBADD)
libpolyorb_soap_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali $(GPERF_BODY)

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-soap.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. \
	-I$(srcdir)/../corba $(XMLADA_INCLUDE) $(GNATFLAGS)
# XXX CORBA included temporarily because of dep of P.Binding_Data.SOAP on
# P.Representations.CDR.

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
	$(INSTALL) -m 444 $(srcdir)/*.ad[bs] $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 *.ali $(DESTDIR)$(alidir)

depend-local: all
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. -I$(srcdir)/../corba $(srcdir)
# XXX dep on CORBA is necessary because of P.R.IOR.

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
aws.lo:aws.ads 
aws-mime.lo:aws.ads aws-mime.adb aws-mime.ads 
aws-response.lo:aws.ads aws-mime.ads aws-response.adb aws-response.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads polyorb-filters-http.ads polyorb-http_methods.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads 
aws-url.lo:aws.ads aws-url.adb aws-url.ads 
polyorb-binding_data-soap.lo:aws.ads aws-url.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads polyorb-binding_data-soap.adb polyorb-binding_data-soap.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-configuration.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads polyorb-filters-http.ads polyorb-http_methods.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-orb-interface.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-soap_pr.ads ./../polyorb-references.ads ./../polyorb-references-ior.ads ./../polyorb-representations.ads ./../polyorb-representations-cdr.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-transport-sockets.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads soap.ads soap-message.ads soap-message-payload.ads soap-parameters.ads 
polyorb-buffer_sources.lo:./../polyorb.ads ./../polyorb-buffers.ads polyorb-buffer_sources.adb polyorb-buffer_sources.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-sockets.ads ./../polyorb-utils.ads ./../polyorb-utils-text_buffers.ads 
polyorb-filters-aws_interface.lo:aws.ads aws-mime.ads aws-response.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads polyorb-filters-aws_interface.ads polyorb-filters-http.ads ./../polyorb-filters-interface.ads polyorb-http_methods.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads 
polyorb-filters-http.lo:aws.ads aws-mime.ads aws-response.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads polyorb-filters-aws_interface.ads polyorb-filters-http.adb polyorb-filters-http.ads ./../polyorb-filters-interface.ads polyorb-http_headers.ads polyorb-http_methods.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-text_buffers.ads 
polyorb-http_headers.lo:./../polyorb.ads polyorb-http_headers.adb polyorb-http_headers.ads ./../polyorb-initialization.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads 
polyorb-http_methods.lo:./../polyorb.ads polyorb-http_methods.adb polyorb-http_methods.ads ./../polyorb-initialization.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads 
polyorb-protocols-soap_pr.lo:aws.ads aws-mime.ads aws-response.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-binding_data-local.ads polyorb-binding_data-soap.ads ./../polyorb-buffers.ads polyorb-buffer_sources.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads polyorb-filters-aws_interface.ads polyorb-filters-http.ads ./../polyorb-filters-interface.ads polyorb-http_methods.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-objects-interface.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-orb-interface.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-soap_pr.adb polyorb-protocols-soap_pr.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads soap.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-message-xml.ads soap-parameters.ads 
polyorb-setup-tcp_access_points-soap.lo:./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads polyorb-binding_data-soap.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-configuration.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads polyorb-filters-http.ads polyorb-http_methods.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-soap_pr.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-setup.ads ./../polyorb-setup-tcp_access_points.ads polyorb-setup-tcp_access_points-soap.adb polyorb-setup-tcp_access_points-soap.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-transport-sockets.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads soap.ads soap-message.ads soap-message-payload.ads soap-parameters.ads 
soap.lo:soap.ads 
soap-message.lo:aws.ads aws-mime.ads aws-response.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads polyorb-filters-http.ads polyorb-http_methods.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads soap.ads soap-message.adb soap-message.ads soap-message-payload.ads soap-message-response.ads soap-parameters.ads soap-types.ads soap-utils.ads 
soap-message-payload.lo:./../polyorb.ads ./../polyorb-any.ads ./../polyorb-any-nvlist.ads ./../polyorb-locks.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-smart_pointers.ads ./../polyorb-soft_links.ads ./../polyorb-types.ads soap.ads soap-message.ads soap-message-payload.adb soap-message-payload.ads soap-parameters.ads 
soap-message-reader.lo:./../polyorb.ads ./../polyorb-any.ads ./../polyorb-any-nvlist.ads ./../polyorb-locks.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-smart_pointers.ads ./../polyorb-soft_links.ads ./../polyorb-types.ads soap.ads soap-message.ads soap-message-reader.adb soap-message-reader.ads soap-parameters.ads 
soap-message-response.lo:aws.ads aws-mime.ads aws-response.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads polyorb-filters-http.ads polyorb-http_methods.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads soap.ads soap-message.ads soap-message-payload.ads soap-message-response.adb soap-message-response.ads soap-message-xml.ads soap-parameters.ads 
soap-message-response-error.lo:aws.ads aws-mime.ads aws-response.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads polyorb-filters-http.ads polyorb-http_methods.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads soap.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-message-response-error.adb soap-message-response-error.ads soap-parameters.ads soap-types.ads soap-utils.ads 
soap-message-xml.lo:aws.ads aws-mime.ads aws-response.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any-objref.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads polyorb-binding_data-soap.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads polyorb-filters-http.ads polyorb-http_methods.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads soap.ads soap-message.ads soap-message-payload.ads soap-message-reader.ads soap-message-response.ads soap-message-response-error.ads soap-message-xml.adb soap-message-xml.ads soap-parameters.ads soap-types.ads 
soap-parameters.lo:./../polyorb.ads ./../polyorb-any.ads ./../polyorb-any-nvlist.ads ./../polyorb-locks.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-smart_pointers.ads ./../polyorb-soft_links.ads ./../polyorb-types.ads soap.ads soap-parameters.adb soap-parameters.ads soap-types.ads 
soap-types.lo:./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any-objref.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads polyorb-binding_data-soap.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-references.ads ./../polyorb-references-binding.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads soap.ads soap-types.adb soap-types.ads soap-utils.ads 
soap-utils.lo:soap.ads soap-utils.adb soap-utils.ads 
