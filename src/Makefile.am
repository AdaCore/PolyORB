if ENABLE_CORBA
CORBA_DIR = corba 
endif

if ENABLE_GIOP
GIOP_DIR = 
endif

if ENABLE_MOMA
MOMA_DIR = moma
endif

if ENABLE_SOAP
SOAP_DIR = soap
endif

if ENABLE_SRP
SRP_DIR = srp
endif

SUBDIRS = . $(CORBA_DIR) $(GIOP_DIR) $(MOMA_DIR) $(SOAP_DIR) $(SRP_DIR)

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
		polyorb-filters.ads \
		polyorb-filters-interface.ads \
		polyorb-filters-slicers.ads \
		polyorb-full_tasking-monitors.ads \
		polyorb-full_tasking-threads.ads \
		polyorb-jobs.ads \
		polyorb-initialization.ads \
		polyorb-locked_queue.ads \
		polyorb-locks.ads \
		polyorb-log.ads \
		polyorb-no_tasking-monitors.ads \
		polyorb-no_tasking-threads.ads \
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
		polyorb-poa_config-proxies.ads \
		polyorb-poa_manager-basic_manager.ads \
		polyorb-poa_policies-id_assignment_policy-system.ads \
		polyorb-poa_policies-id_assignment_policy-user.ads \
		polyorb-poa_policies-id_uniqueness_policy-multiple.ads \
		polyorb-poa_policies-id_uniqueness_policy-unique.ads \
		polyorb-poa_policies-implicit_activation_policy-no_activation.ads \
		polyorb-poa_policies-lifespan_policy-transient.ads \
		polyorb-poa_policies-lifespan_policy-persistent.ads \
		polyorb-poa_policies-request_processing_policy-active_object_map_only.ads \
		polyorb-poa_policies-request_processing_policy-use_default_servant.ads \
		polyorb-poa_policies-servant_retention_policy-non_retain.ads \
		polyorb-poa_policies-servant_retention_policy-retain.ads \
		polyorb-poa_policies-thread_policy-orb_ctrl.ads \
		polyorb-poa_types.ads \
		polyorb-protected_objects.ads \
		polyorb-protocols.ads \
		polyorb-protocols-echo.ads \
		polyorb-ravenscar-index_manager.ads \
		polyorb-ravenscar-monitors.ads \
		polyorb-ravenscar-threads.ads \
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
		polyorb-setup-tcp_access_points.ads \
		polyorb-smart_pointers.ads \
		polyorb-soft_links.ads \
		polyorb-static_dict.ads \
		polyorb-tasking-soft_links.ads \
		polyorb-tasking-watchers.ads \
		polyorb-tasking-mutexes.ads \
		polyorb-tasking-threads.ads \
		polyorb-tasking-monitors.ads \
		polyorb-task_info.ads \
		polyorb-transport.ads \
		polyorb-transport-sockets.ads \
		polyorb-types.ads \
		polyorb-utils.ads \
		polyorb-utils-buffers.ads \
		polyorb-utils-chained_lists.ads \
		polyorb-utils-dynamic_tables.ads \
		polyorb-utils-htables.ads \
		polyorb-utils-htables-perfect.ads \
		polyorb-utils-semaphores.ads \
		polyorb-utils-simple_flags.ads \
		polyorb-utils-strings.ads \
		polyorb-utils-strings-lists.ads \
		polyorb-utils-text_buffers.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
		polyorb.ads \
		polyorb-constants.ads \
		polyorb-full_tasking.ads \
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
		polyorb-ravenscar.ads \
		polyorb-ravenscar-configuration.ads \
		polyorb-representations.ads \
		polyorb-sequences.ads \
		polyorb-setup.ads \
		polyorb-sockets.ads \
		polyorb-storage_pools.ads \
		polyorb-tasking.ads \
		polyorb-test.ads

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
	$(INSTALL) -m 444 $(srcdir)/*.ad[bs] $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 *.ali $(DESTDIR)$(alidir)

depend:
	for d in $(SUBDIRS);do \
		(cd $$d && ${MAKE} depend-local); \
	done

depend-local: all
	$(srcdir)/../support/mkdep $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
