corba_dir=	corba 
#dsa_dir=	dsa
giop_dir=	giop
moma_dir=	moma
soap_dir=	soap
srp_dir=	srp

SUBDIRS = . @PROTO_DIRS@ @APPLI_DIRS@

include $(srcdir)/Makefile.common

alidir = $(libdir)/polyorb

ADA_SPECS_WITH_BODY = \
		polyorb-annotations.ads \
		polyorb-any.ads \
		polyorb-any-exceptionlist.ads \
		polyorb-any-nvlist.ads \
		polyorb-any-objref.ads \
		polyorb-asynch_ev.ads \
		polyorb-asynch_ev-sockets.ads \
		polyorb-binding_data.ads \
		polyorb-binding_data-local.ads \
		polyorb-buffers.ads \
		polyorb-call_back.ads \
		polyorb-components.ads \
		polyorb-configuration.ads \
		polyorb-dynamic_dict.ads \
		polyorb-exceptions.ads \
		polyorb-exceptions-stack.ads \
		polyorb-filters.ads \
		polyorb-filters-interface.ads \
		polyorb-filters-slicers.ads \
		polyorb-fixed_point.ads \
		polyorb-jobs.ads \
		polyorb-initialization.ads \
		polyorb-log.ads \
		polyorb-minimal_servant.ads \
		polyorb-minimal_servant-tools.ads \
		polyorb-obj_adapters.ads \
		polyorb-obj_adapters-simple.ads \
		polyorb-object_maps.ads \
		polyorb-objects.ads \
		polyorb-opaque.ads \
		polyorb-opaque-chunk_pools.ads \
		polyorb-orb.ads \
		polyorb-orb-no_tasking.ads \
		polyorb-orb-thread_per_request.ads \
		polyorb-orb-thread_per_session.ads \
		polyorb-orb-thread_pool.ads \
		polyorb-poa-basic_poa.ads \
		polyorb-poa.ads \
		polyorb-poa_config.ads \
		polyorb-poa_config-minimum.ads \
		polyorb-poa_config-root_poa.ads \
		polyorb-poa_config-proxies.ads \
		polyorb-poa_manager-basic_manager.ads \
		polyorb-poa_policies-id_assignment_policy-system.ads \
		polyorb-poa_policies-id_assignment_policy-user.ads \
		polyorb-poa_policies-id_uniqueness_policy-multiple.ads \
		polyorb-poa_policies-id_uniqueness_policy-unique.ads \
		polyorb-poa_policies-implicit_activation_policy-activation.ads \
		polyorb-poa_policies-implicit_activation_policy-no_activation.ads \
		polyorb-poa_policies-lifespan_policy-transient.ads \
		polyorb-poa_policies-lifespan_policy-persistent.ads \
		polyorb-poa_policies-request_processing_policy-active_object_map_only.ads \
		polyorb-poa_policies-request_processing_policy-use_default_servant.ads \
		polyorb-poa_policies-servant_retention_policy-non_retain.ads \
		polyorb-poa_policies-servant_retention_policy-retain.ads \
		polyorb-poa_policies-thread_policy-orb_ctrl.ads \
		polyorb-poa_policies-thread_policy-main_thread.ads \
		polyorb-poa_policies-thread_policy-single_thread.ads \
		polyorb-poa_types.ads \
		polyorb-profiles-full_tasking.ads \
		polyorb-profiles-no_tasking.ads \
		polyorb-protocols.ads \
		polyorb-protocols-echo.ads \
		polyorb-references.ads \
		polyorb-references-binding.ads \
		polyorb-references-ior.ads \
		polyorb-representations-cdr.ads \
		polyorb-representations-test.ads \
		polyorb-requests.ads \
		polyorb-sequences-bounded.ads \
		polyorb-sequences-unbounded.ads \
		polyorb-sequences-unbounded-search.ads \
		polyorb-servants.ads \
		polyorb-setup-proxies_poa.ads \
		polyorb-setup-tcp_access_points.ads \
		polyorb-smart_pointers.ads \
		polyorb-tasking-advanced_mutexes.ads \
		polyorb-tasking-mutexes.ads \
		polyorb-tasking-condition_variables.ads \
		polyorb-tasking-profiles-full_tasking-condition_variables.ads \
		polyorb-tasking-profiles-full_tasking-mutexes.ads \
		polyorb-tasking-profiles-full_tasking-threads.ads \
		polyorb-tasking-profiles-no_tasking-condition_variables.ads \
		polyorb-tasking-profiles-no_tasking-mutexes.ads \
		polyorb-tasking-profiles-no_tasking-threads.ads \
		polyorb-tasking-profiles-ravenscar-condition_variables.ads \
		polyorb-tasking-profiles-ravenscar-index_manager.ads \
		polyorb-tasking-profiles-ravenscar-mutexes.ads \
		polyorb-tasking-profiles-ravenscar-threads.ads \
		polyorb-tasking-rw_locks.ads \
		polyorb-tasking-semaphores.ads \
		polyorb-tasking-soft_links.ads \
		polyorb-tasking-threads.ads \
		polyorb-tasking-watchers.ads \
		polyorb-task_info.ads \
		polyorb-transport.ads \
		polyorb-transport-sockets.ads \
		polyorb-types.ads \
		polyorb-utils.ads \
		polyorb-utils-buffers.ads \
		polyorb-utils-chained_lists.ads \
		polyorb-utils-dynamic_tables.ads \
		polyorb-utils-htables-perfect.ads \
		polyorb-utils-simple_flags.ads \
		polyorb-utils-strings.ads \
		polyorb-utils-strings-lists.ads \
		polyorb-utils-text_buffers.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
		polyorb.ads \
		polyorb-constants.ads \
		polyorb-if_descriptors.ads \
		polyorb-objects-interface.ads \
		polyorb-orb-interface.ads \
		polyorb-poa_manager.ads \
		polyorb-poa_policies.ads \
		polyorb-poa_policies-id_assignment_policy.ads \
		polyorb-poa_policies-id_uniqueness_policy.ads \
		polyorb-poa_policies-implicit_activation_policy.ads \
		polyorb-poa_policies-lifespan_policy.ads \
		polyorb-poa_policies-request_processing_policy.ads \
		polyorb-poa_policies-servant_retention_policy.ads \
		polyorb-poa_policies-thread_policy.ads \
		polyorb-protocols-interface.ads \
		polyorb-profiles.ads \
		polyorb-profiles-ravenscar.ads \
		polyorb-representations.ads \
		polyorb-sequences.ads \
		polyorb-setup.ads \
		polyorb-sockets.ads \
		polyorb-storage_pools.ads \
		polyorb-tasking.ads \
		polyorb-tasking-priorities.ads \
		polyorb-tasking-profiles.ads \
		polyorb-tasking-profiles-ravenscar.ads \
		polyorb-tasking-profiles-ravenscar-configuration.ads \
		polyorb-tasking-profiles-full_tasking.ads \
		polyorb-tasking-profiles-no_tasking.ads \
		polyorb-utils-htables.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

#bin_SCRIPTS = polyorb-config
lib_LTLIBRARIES = libpolyorb.la
libpolyorb_la_SOURCES =
libpolyorb_la_LIBADD = $(ADA_LIBADD)
libpolyorb_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb.la

ADAFLAGS = $(GNATFLAGS) \
	-I. -I$(srcdir)

ADACOMPILER = ../support/adacompiler

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

depend:
	for d in $(SUBDIRS);do \
		(cd $$d && ${MAKE} depend-local); \
	done

depend-local: all
	$(srcdir)/../support/mkdep $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb.lo:polyorb.ads 
polyorb-annotations.lo:polyorb.ads polyorb-annotations.adb polyorb-annotations.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-any.lo:polyorb.ads polyorb-any.adb polyorb-any.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-types.adb polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-any-exceptionlist.lo:polyorb.ads polyorb-any.ads polyorb-any-exceptionlist.adb polyorb-any-exceptionlist.ads polyorb-log.adb polyorb-log.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-sequences-unbounded-search.adb polyorb-sequences-unbounded-search.ads polyorb-smart_pointers.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-types.ads 
polyorb-any-nvlist.lo:polyorb.ads polyorb-any.ads polyorb-any-nvlist.adb polyorb-any-nvlist.ads polyorb-log.adb polyorb-log.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-types.ads 
polyorb-any-objref.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-objref.adb polyorb-any-objref.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-references.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads 
polyorb-asynch_ev.lo:polyorb.ads polyorb-annotations.ads polyorb-asynch_ev.adb polyorb-asynch_ev.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-asynch_ev-sockets.lo:polyorb.ads polyorb-annotations.ads polyorb-asynch_ev.ads polyorb-asynch_ev-sockets.adb polyorb-asynch_ev-sockets.ads polyorb-constants.ads polyorb-log.adb polyorb-log.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-sockets.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-binding_data.lo:polyorb.ads polyorb-annotations.ads polyorb-asynch_ev.ads polyorb-binding_data.adb polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-log.adb polyorb-log.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-binding_data-iiop.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-configuration.ads polyorb-filters.ads polyorb-filters-slicers.ads polyorb-initialization.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.adb polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-protocols.ads polyorb-references.ads polyorb-references-ior.ads polyorb-representations.ads polyorb-representations-cdr.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-transport-sockets.ads polyorb-types.adb polyorb-types.ads polyorb-utils.adb polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.adb polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-binding_data-local.lo:polyorb.ads polyorb-annotations.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-binding_data-local.adb polyorb-binding_data-local.ads polyorb-buffers.ads polyorb-components.ads polyorb-objects.adb polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.adb polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-buffers.lo:polyorb.ads polyorb-buffers.adb polyorb-buffers.ads polyorb-log.adb polyorb-log.ads polyorb-opaque.adb polyorb-opaque.ads polyorb-opaque-chunk_pools.adb polyorb-opaque-chunk_pools.ads polyorb-sockets.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-components.lo:polyorb.ads polyorb-components.adb polyorb-components.ads polyorb-log.adb polyorb-log.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads 
polyorb-configuration.lo:polyorb.ads polyorb-configuration.adb polyorb-configuration.ads polyorb-dynamic_dict.adb polyorb-dynamic_dict.ads polyorb-log.ads polyorb-utils.adb polyorb-utils.ads polyorb-utils-dynamic_tables.adb polyorb-utils-dynamic_tables.ads polyorb-utils-htables.ads polyorb-utils-htables-perfect.adb polyorb-utils-htables-perfect.ads polyorb-utils-strings.adb polyorb-utils-strings.ads 
polyorb-constants.lo:polyorb.ads polyorb-constants.ads 
polyorb-dynamic_dict.lo:polyorb.ads polyorb-dynamic_dict.adb polyorb-dynamic_dict.ads polyorb-utils.ads polyorb-utils-dynamic_tables.adb polyorb-utils-dynamic_tables.ads polyorb-utils-htables.ads polyorb-utils-htables-perfect.adb polyorb-utils-htables-perfect.ads polyorb-utils-strings.ads 
polyorb-exceptions.lo:polyorb.ads polyorb-any.ads polyorb-exceptions.adb polyorb-exceptions.ads polyorb-exceptions-stack.ads polyorb-initialization.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-types.adb polyorb-types.ads polyorb-utils.adb polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.adb polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-exceptions-stack.lo:polyorb.ads polyorb-any.ads polyorb-exceptions.ads polyorb-exceptions-stack.adb polyorb-exceptions-stack.ads polyorb-initialization.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.adb polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-filters.lo:polyorb.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.adb polyorb-filters.ads polyorb-filters-interface.ads polyorb-log.adb polyorb-log.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-sockets.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-filters-interface.lo:polyorb.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-filters-interface.adb polyorb-filters-interface.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-sockets.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-filters-slicers.lo:polyorb.ads polyorb-buffers.adb polyorb-buffers.ads polyorb-components.adb polyorb-components.ads polyorb-filters.ads polyorb-filters-interface.ads polyorb-filters-slicers.adb polyorb-filters-slicers.ads polyorb-log.adb polyorb-log.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-sockets.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-giop_p.lo:polyorb.ads 
polyorb-giop_p-exceptions.lo:polyorb.ads polyorb-any.ads polyorb-exceptions.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-types.adb polyorb-types.ads polyorb-utils.adb polyorb-utils.ads 
polyorb-if_descriptors.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-if_descriptors.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-references.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads 
polyorb-initialization.lo:polyorb.ads polyorb-configuration.ads polyorb-dynamic_dict.adb polyorb-dynamic_dict.ads polyorb-initialization.adb polyorb-initialization.ads polyorb-log.adb polyorb-log.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-dynamic_tables.adb polyorb-utils-dynamic_tables.ads polyorb-utils-htables.ads polyorb-utils-htables-perfect.adb polyorb-utils-htables-perfect.ads polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-jobs.lo:polyorb.ads polyorb-jobs.adb polyorb-jobs.ads 
polyorb-log.lo:polyorb.ads polyorb-log.adb polyorb-log.ads 
polyorb-obj_adapters.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-obj_adapters.adb polyorb-obj_adapters.ads polyorb-objects.adb polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-references.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-transport.ads polyorb-types.adb polyorb-types.ads polyorb-utils.adb polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads 
polyorb-objects.lo:polyorb.ads polyorb-objects.adb polyorb-objects.ads polyorb-utils.adb polyorb-utils.ads 
polyorb-objects-interface.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-objects.ads polyorb-objects-interface.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-opaque.lo:polyorb.ads polyorb-opaque.adb polyorb-opaque.ads 
polyorb-opaque-chunk_pools.lo:polyorb.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.adb polyorb-opaque-chunk_pools.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-orb.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.adb polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-binding_data-local.ads polyorb-buffers.ads polyorb-components.ads polyorb-constants.ads polyorb-filters.ads polyorb-filters-interface.ads polyorb-initialization.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-objects-interface.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.adb polyorb-orb.ads polyorb-orb-interface.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-references.ads polyorb-references-binding.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-setup.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.adb polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.adb polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.adb polyorb-utils-simple_flags.ads polyorb-utils-strings.adb polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-orb-interface.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-jobs.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-orb-interface.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-orb-thread_pool.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-configuration.ads polyorb-filters.ads polyorb-filters-interface.ads polyorb-initialization.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-orb-thread_pool.adb polyorb-orb-thread_pool.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-setup.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.adb polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-poa_policies.lo:polyorb.ads polyorb-objects.ads polyorb-poa_policies.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads 
polyorb-poa_policies-thread_policy.lo:polyorb.ads polyorb-components.ads polyorb-objects.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads 
polyorb-profiles.lo:polyorb.ads polyorb-profiles.ads 
polyorb-profiles-ravenscar.lo:polyorb.ads polyorb-initialization.ads polyorb-log.adb polyorb-log.ads polyorb-profiles.ads polyorb-profiles-ravenscar.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-profiles.ads polyorb-tasking-profiles-ravenscar.ads polyorb-tasking-profiles-ravenscar-condition_variables.adb polyorb-tasking-profiles-ravenscar-condition_variables.ads polyorb-tasking-profiles-ravenscar-index_manager.adb polyorb-tasking-profiles-ravenscar-index_manager.ads polyorb-tasking-profiles-ravenscar-mutexes.adb polyorb-tasking-profiles-ravenscar-mutexes.ads polyorb-tasking-profiles-ravenscar-threads.adb polyorb-tasking-profiles-ravenscar-threads.ads polyorb-tasking-threads.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads polyorb-utils-strings-lists.ads 
polyorb-protocols.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-filters-interface.ads polyorb-if_descriptors.ads polyorb-log.adb polyorb-log.ads polyorb-objects.ads polyorb-objects-interface.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-protocols.adb polyorb-protocols.ads polyorb-protocols-interface.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.adb polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.adb polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-protocols-giop.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.adb polyorb-binding_data.ads polyorb-binding_data-local.ads polyorb-buffers.adb polyorb-buffers.ads polyorb-components.adb polyorb-components.ads polyorb-exceptions.ads polyorb-filters.ads polyorb-filters-interface.ads polyorb-initialization.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.adb polyorb-objects.ads polyorb-objects-interface.ads polyorb-opaque.adb polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-orb-interface.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-protocols.ads polyorb-references.ads polyorb-references-ior.ads polyorb-representations.ads polyorb-representations-cdr.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.adb polyorb-types.ads polyorb-utils.adb polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.adb polyorb-utils-simple_flags.ads polyorb-utils-strings.adb polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-protocols-giop-giop_1_0.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-protocols.ads polyorb-references.ads polyorb-references-ior.ads polyorb-representations.ads polyorb-representations-cdr.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-protocols-giop-giop_1_1.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-protocols.ads polyorb-references.ads polyorb-references-ior.ads polyorb-representations.ads polyorb-representations-cdr.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-protocols-giop-giop_1_2.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-protocols.ads polyorb-references.ads polyorb-references-ior.ads polyorb-representations.ads polyorb-representations-cdr.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-protocols-interface.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-protocols.ads polyorb-protocols-interface.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-references.lo:polyorb.ads polyorb-annotations.ads polyorb-asynch_ev.ads polyorb-binding_data.adb polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-filters-interface.ads polyorb-log.adb polyorb-log.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-references.adb polyorb-references.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads 
polyorb-references-binding.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-binding_data-local.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-references.ads polyorb-references-binding.adb polyorb-references-binding.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-setup.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-references-ior.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-log.adb polyorb-log.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-references.ads polyorb-references-ior.adb polyorb-references-ior.ads polyorb-representations.ads polyorb-representations-cdr.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-transport.ads polyorb-types.adb polyorb-types.ads polyorb-utils.adb polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads 
polyorb-representations.lo:polyorb.ads polyorb-any.ads polyorb-buffers.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-representations.ads polyorb-sockets.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-representations-cdr.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-objref.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-log.adb polyorb-log.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-references.ads polyorb-references-ior.ads polyorb-representations.ads polyorb-representations-cdr.adb polyorb-representations-cdr.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-transport.ads polyorb-types.adb polyorb-types.ads polyorb-utils.ads polyorb-utils-buffers.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads 
polyorb-requests.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-orb-interface.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-protocols.ads polyorb-protocols-interface.ads polyorb-references.ads polyorb-requests.adb polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-setup.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.adb polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-sequences.lo:polyorb.ads polyorb-sequences.ads 
polyorb-sequences-unbounded.lo:polyorb.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads 
polyorb-sequences-unbounded-search.lo:polyorb.ads polyorb-sequences.ads polyorb-sequences-unbounded.ads polyorb-sequences-unbounded-search.adb polyorb-sequences-unbounded-search.ads 
polyorb-servants.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-log.adb polyorb-log.ads polyorb-objects.ads polyorb-objects-interface.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.adb polyorb-servants.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-setup.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-jobs.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-setup.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-setup-ravenscar_tp_server.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-initialization.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-orb-thread_pool.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-profiles.ads polyorb-profiles-ravenscar.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-setup.ads polyorb-setup-ravenscar_tp_server.adb polyorb-setup-ravenscar_tp_server.ads polyorb-setup-server.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-profiles.ads polyorb-tasking-profiles-ravenscar.ads polyorb-tasking-profiles-ravenscar-condition_variables.adb polyorb-tasking-profiles-ravenscar-condition_variables.ads polyorb-tasking-profiles-ravenscar-index_manager.adb polyorb-tasking-profiles-ravenscar-index_manager.ads polyorb-tasking-profiles-ravenscar-mutexes.adb polyorb-tasking-profiles-ravenscar-mutexes.ads polyorb-tasking-profiles-ravenscar-threads.adb polyorb-tasking-profiles-ravenscar-threads.ads polyorb-tasking-soft_links.ads polyorb-tasking-threads.adb polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.adb polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-setup-server.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-jobs.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-setup.ads polyorb-setup-server.ads polyorb-setup-tcp_access_points.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-setup-tcp_access_points.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-jobs.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-setup.ads polyorb-setup-tcp_access_points.adb polyorb-setup-tcp_access_points.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-transport-sockets.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-setup-tcp_access_points-iiop.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-configuration.ads polyorb-filters.ads polyorb-filters-slicers.ads polyorb-initialization.ads polyorb-jobs.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-protocols.ads polyorb-references.ads polyorb-references-ior.ads polyorb-representations.ads polyorb-representations-cdr.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-setup.ads polyorb-setup-tcp_access_points.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.ads polyorb-transport-sockets.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.adb polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-smart_pointers.lo:polyorb.ads polyorb-initialization.ads polyorb-log.adb polyorb-log.ads polyorb-smart_pointers.adb polyorb-smart_pointers.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.adb polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-sockets.lo:polyorb.ads polyorb-sockets.ads 
polyorb-task_info.lo:polyorb.ads polyorb-annotations.ads polyorb-asynch_ev.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-task_info.adb polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-tasking-advanced_mutexes.lo:polyorb.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-advanced_mutexes.adb polyorb-tasking-advanced_mutexes.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads 
polyorb-tasking.lo:polyorb.ads polyorb-tasking.ads 
polyorb-tasking-condition_variables.lo:polyorb.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-condition_variables.adb polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads 
polyorb-tasking-mutexes.lo:polyorb.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-mutexes.adb polyorb-tasking-mutexes.ads 
polyorb-tasking-profiles.lo:polyorb.ads polyorb-tasking.ads polyorb-tasking-profiles.ads 
polyorb-tasking-profiles-ravenscar.lo:polyorb.ads polyorb-tasking.ads polyorb-tasking-profiles.ads polyorb-tasking-profiles-ravenscar.ads 
polyorb-tasking-profiles-ravenscar-condition_variables.lo:polyorb.ads polyorb-initialization.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-profiles.ads polyorb-tasking-profiles-ravenscar.ads polyorb-tasking-profiles-ravenscar-condition_variables.adb polyorb-tasking-profiles-ravenscar-condition_variables.ads polyorb-tasking-profiles-ravenscar-index_manager.adb polyorb-tasking-profiles-ravenscar-index_manager.ads polyorb-tasking-profiles-ravenscar-threads.adb polyorb-tasking-profiles-ravenscar-threads.ads polyorb-tasking-threads.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads polyorb-utils-strings-lists.ads 
polyorb-tasking-profiles-ravenscar-index_manager.lo:polyorb.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-profiles.ads polyorb-tasking-profiles-ravenscar.ads polyorb-tasking-profiles-ravenscar-index_manager.adb polyorb-tasking-profiles-ravenscar-index_manager.ads 
polyorb-tasking-profiles-ravenscar-mutexes.lo:polyorb.ads polyorb-initialization.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-tasking-profiles.ads polyorb-tasking-profiles-ravenscar.ads polyorb-tasking-profiles-ravenscar-index_manager.adb polyorb-tasking-profiles-ravenscar-index_manager.ads polyorb-tasking-profiles-ravenscar-mutexes.adb polyorb-tasking-profiles-ravenscar-mutexes.ads polyorb-tasking-profiles-ravenscar-threads.ads polyorb-tasking-threads.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads polyorb-utils-strings-lists.ads 
polyorb-tasking-profiles-ravenscar-threads.lo:polyorb.ads polyorb-initialization.ads polyorb-log.adb polyorb-log.ads polyorb-tasking.ads polyorb-tasking-profiles.ads polyorb-tasking-profiles-ravenscar.ads polyorb-tasking-profiles-ravenscar-index_manager.adb polyorb-tasking-profiles-ravenscar-index_manager.ads polyorb-tasking-profiles-ravenscar-threads.adb polyorb-tasking-profiles-ravenscar-threads.ads polyorb-tasking-threads.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads polyorb-utils-strings-lists.ads 
polyorb-tasking-soft_links.lo:polyorb.ads polyorb-initialization.ads polyorb-tasking.ads polyorb-tasking-advanced_mutexes.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-soft_links.adb polyorb-tasking-soft_links.ads polyorb-tasking-threads.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.adb polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
polyorb-tasking-threads.lo:polyorb.ads polyorb-tasking.ads polyorb-tasking-threads.adb polyorb-tasking-threads.ads 
polyorb-transport.lo:polyorb.ads polyorb-annotations.ads polyorb-any.ads polyorb-any-exceptionlist.ads polyorb-any-nvlist.ads polyorb-asynch_ev.ads polyorb-binding_data.ads polyorb-buffers.ads polyorb-components.ads polyorb-filters.ads polyorb-filters-interface.ads polyorb-jobs.ads polyorb-log.adb polyorb-log.ads polyorb-obj_adapters.ads polyorb-objects.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-orb.ads polyorb-orb-interface.ads polyorb-poa_policies.ads polyorb-poa_policies-thread_policy.ads polyorb-references.ads polyorb-requests.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-servants.ads polyorb-smart_pointers.ads polyorb-sockets.ads polyorb-task_info.ads polyorb-tasking.ads polyorb-tasking-condition_variables.ads polyorb-tasking-mutexes.ads polyorb-tasking-threads.ads polyorb-transport.adb polyorb-transport.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-simple_flags.ads polyorb-utils-strings.ads 
polyorb-transport-sockets.lo:polyorb.ads polyorb-annotations.ads polyorb-asynch_ev.ads polyorb-asynch_ev-sockets.ads polyorb-buffers.ads polyorb-components.ads polyorb-log.adb polyorb-log.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-sequences.ads polyorb-sequences-unbounded.adb polyorb-sequences-unbounded.ads polyorb-sockets.ads polyorb-tasking.ads polyorb-tasking-mutexes.ads polyorb-transport.ads polyorb-transport-sockets.adb polyorb-transport-sockets.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-types.lo:polyorb.ads polyorb-types.adb polyorb-types.ads 
polyorb-utils.lo:polyorb.ads polyorb-utils.adb polyorb-utils.ads 
polyorb-utils-buffers.lo:polyorb.ads polyorb-buffers.ads polyorb-opaque.ads polyorb-opaque-chunk_pools.ads polyorb-sockets.ads polyorb-utils.ads polyorb-utils-buffers.adb polyorb-utils-buffers.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-utils-chained_lists.lo:polyorb.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads 
polyorb-utils-dynamic_tables.lo:polyorb.ads polyorb-utils.ads polyorb-utils-dynamic_tables.adb polyorb-utils-dynamic_tables.ads 
polyorb-utils-htables.lo:polyorb.ads polyorb-utils.ads polyorb-utils-htables.ads 
polyorb-utils-htables-perfect.lo:polyorb.ads polyorb-utils.ads polyorb-utils-dynamic_tables.adb polyorb-utils-dynamic_tables.ads polyorb-utils-htables.ads polyorb-utils-htables-perfect.adb polyorb-utils-htables-perfect.ads polyorb-utils-strings.ads 
polyorb-utils-simple_flags.lo:polyorb.ads polyorb-log.adb polyorb-log.ads polyorb-types.ads polyorb-utils.ads polyorb-utils-simple_flags.adb polyorb-utils-simple_flags.ads 
polyorb-utils-strings.lo:polyorb.ads polyorb-utils.ads polyorb-utils-strings.adb polyorb-utils-strings.ads 
polyorb-utils-strings-lists.lo:polyorb.ads polyorb-utils.ads polyorb-utils-chained_lists.adb polyorb-utils-chained_lists.ads polyorb-utils-strings.ads polyorb-utils-strings-lists.adb polyorb-utils-strings-lists.ads 
