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
aws-client.lo:aws-client.adb aws-client.ads aws-headers.ads aws-headers-set.adb aws-headers-set.ads aws-headers-values.ads aws-response.adb aws-response.ads aws-response-set.adb aws-response-set.ads aws-status.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.adb ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-exceptions.ads ./../polyorb-filters.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-references.ads ./../polyorb-references-uri.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-task_info.ads ./../polyorb-tasking.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-threads.ads ./../polyorb-transport.ads ./../polyorb-types.adb ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-parameters.ads soap-types.ads 
aws-exceptions.lo:aws-exceptions.ads aws-headers.ads aws-response.ads aws-status.ads ./../polyorb.ads ./../polyorb-any.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-parameters.ads soap-types.ads 
aws-headers.lo:aws-headers.adb aws-headers.ads 
aws-headers-set.lo:aws-headers.ads aws-headers-set.adb aws-headers-set.ads 
aws-headers-values.lo:aws-headers.ads aws-headers-values.adb aws-headers-values.ads 
aws-parameters-set.lo:aws-parameters-set.adb aws-parameters-set.ads 
aws-response.lo:aws-headers.ads aws-headers-set.adb aws-headers-set.ads aws-headers-values.ads aws-response.adb aws-response.ads aws-response-set.adb aws-response-set.ads aws-status.ads ./../polyorb.ads ./../polyorb-any.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-parameters.ads soap-types.ads 
aws-response-set.lo:aws-headers.ads aws-headers-set.adb aws-headers-set.ads aws-response.ads aws-response-set.adb aws-response-set.ads aws-status.ads ./../polyorb.ads ./../polyorb-any.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-parameters.ads soap-types.ads 
aws-server.lo:aws-exceptions.ads aws-headers.ads aws-object_adapter.ads aws-response.ads aws-server.adb aws-server.ads aws-status.ads aws-status-translate_table.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-exceptions.ads ./../polyorb-filters.ads ./../polyorb-initialization.adb ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-object_maps.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-parameters.ads ./../polyorb-poa.ads ./../polyorb-poa-basic_poa.ads ./../polyorb-poa_config.adb ./../polyorb-poa_config.ads ./../polyorb-poa_config-root_poa.ads ./../polyorb-poa_manager.ads ./../polyorb-poa_manager-basic_manager.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-id_assignment_policy.ads ./../polyorb-poa_policies-id_uniqueness_policy.ads ./../polyorb-poa_policies-id_uniqueness_policy-multiple.ads ./../polyorb-poa_policies-implicit_activation_policy.ads ./../polyorb-poa_policies-implicit_activation_policy-no_activation.ads ./../polyorb-poa_policies-lifespan_policy.ads ./../polyorb-poa_policies-lifespan_policy-persistent.ads ./../polyorb-poa_policies-request_processing_policy.ads ./../polyorb-poa_policies-request_processing_policy-use_default_servant.ads ./../polyorb-poa_policies-servant_retention_policy.ads ./../polyorb-poa_policies-servant_retention_policy-non_retain.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_types.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-scheduler.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-servants-interface.ads ./../polyorb-setup.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-task_info.ads ./../polyorb-tasking.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-threads.ads ./../polyorb-transport.ads ./../polyorb-types.adb ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-dynamic_tables.adb ./../polyorb-utils-dynamic_tables.ads ./../polyorb-utils-hfunctions.ads ./../polyorb-utils-hfunctions-hyper.ads ./../polyorb-utils-htables.ads ./../polyorb-utils-htables-perfect.ads ./../polyorb-utils-simple_flags.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.adb ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.adb ./../polyorb-utils-strings-lists.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-parameters.ads soap-types.ads 
aws-server-get_status.lo:aws-exceptions.ads aws-headers.ads aws-response.ads aws-server.ads aws-server-get_status.adb aws-server-get_status.ads aws-status.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-exceptions.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-references.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-parameters.ads soap-types.ads 
aws-server-servants.lo:aws-exceptions.ads aws-headers.ads aws-headers-set.ads aws-headers-values.ads aws-parameters-set.ads aws-response.adb aws-response.ads aws-response-set.ads aws-server.ads aws-server-servants.adb aws-server-servants.ads aws-status.ads aws-status-set.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-exceptions.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-orb-interface.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-scheduler.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-servants-interface.ads ./../polyorb-setup.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-task_info.ads ./../polyorb-tasking.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-threads.ads ./../polyorb-transport.ads ./../polyorb-types.adb ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-parameters.ads soap-types.ads 
aws-status.lo:aws-headers.ads aws-headers-values.ads aws-status.adb aws-status.ads ./../polyorb.ads ./../polyorb-any.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-message.ads soap-message-payload.ads soap-parameters.ads soap-types.ads 
aws-status-set.lo:aws-headers.ads aws-parameters-set.ads aws-status.ads aws-status-set.adb aws-status-set.ads ./../polyorb.ads ./../polyorb-any.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-message.ads soap-message-payload.ads soap-parameters.ads soap-types.ads 
aws-status-translate_table.lo:aws-headers.ads aws-status.ads aws-status-translate_table.adb aws-status-translate_table.ads ./../polyorb.ads ./../polyorb-any.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-message.ads soap-message-payload.ads soap-parameters.ads soap-types.ads 
soap-client.lo:aws-client.ads aws-headers.ads aws-response.ads aws-status.ads ./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-exceptions.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-references.ads ./../polyorb-references-uri.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-task_info.ads ./../polyorb-tasking.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-threads.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads soap-client.adb soap-client.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-parameters.ads soap-types.ads 
soap-message.lo:./../polyorb.ads ./../polyorb-any.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-message.adb soap-message.ads soap-parameters.ads soap-types.ads 
soap-message-payload.lo:./../polyorb.ads ./../polyorb-any.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-message.ads soap-message-payload.adb soap-message-payload.ads soap-parameters.ads soap-types.ads 
soap-message-response.lo:./../polyorb.ads ./../polyorb-any.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-message.ads soap-message-payload.ads soap-message-response.adb soap-message-response.ads soap-parameters.ads soap-types.ads 
soap-message-response-error.lo:./../polyorb.ads ./../polyorb-any.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-message.ads soap-message-payload.ads soap-message-response.ads soap-message-response-error.adb soap-message-response-error.ads soap-parameters.ads soap-types.ads 
soap-parameters.lo:./../polyorb.ads ./../polyorb-any.ads ./../polyorb-log.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads soap-parameters.adb soap-parameters.ads soap-types.adb soap-types.ads 
soap-types.lo:./../polyorb.ads ./../polyorb-any.adb ./../polyorb-any.ads ./../polyorb-initialization.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.adb ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads soap-types.adb soap-types.ads 
