SUBDIRS = .
# corba soap

alidir = $(libdir)/polyorb

ADA_SPECS_WITH_BODY = \
		polyorb-annotations.ads \
		polyorb-any.ads \
		polyorb-any-nvlist.ads \
		polyorb-any-objref.ads \
		polyorb-asynch_ev.ads \
		polyorb-asynch_ev-sockets.ads \
		polyorb-binding_data.ads \
		polyorb-binding_data-local.ads \
		polyorb-binding_data-srp.ads \
		polyorb-binding_data-test.ads \
		polyorb-buffers.ads \
		polyorb-components.ads \
		polyorb-dict.ads \
		polyorb-dynamic_dict.ads \
		polyorb-filters.ads \
		polyorb-filters-slicers.ads \
		polyorb-jobs.ads \
		polyorb-locked_queue.ads \
		polyorb-locks.ads \
		polyorb-log.ads \
		polyorb-no_tasking.ads \
		polyorb-obj_adapters.ads \
		polyorb-obj_adapters-simple.ads \
		polyorb-object_map.ads \
		polyorb-object_maps-seq.ads \
		polyorb-objects.ads \
		polyorb-opaque.ads \
		polyorb-opaque-chunk_pools.ads \
		polyorb-orb.ads \
		polyorb-orb-task_policies.ads \
		polyorb-orb-thread_per_session.ads \
		polyorb-orb-thread_pool.ads \
		polyorb-poa-basic_poa.ads \
		polyorb-poa_config.ads \
		polyorb-poa_config-minimum.ads \
		polyorb-poa_manager-basic_manager.ads \
		polyorb-poa_policies-id_assignment_policy-system.ads \
		polyorb-poa_policies-id_uniqueness_policy-unique.ads \
		polyorb-poa_policies-implicit_activation_policy-no_activation.ads \
		polyorb-poa_policies-lifespan_policy-transient.ads \
		polyorb-poa_policies-request_processing_policy-active_object_map_only.ads \
		polyorb-poa_policies-servant_retention_policy-retain.ads \
		polyorb-poa_policies-thread_policy-orb_ctrl.ads \
		polyorb-poa_types.ads \
		polyorb-protected_objects.ads \
		polyorb-protocols.ads \
		polyorb-protocols-echo.ads \
		polyorb-protocols-srp.ads \
		polyorb-references.ads \
		polyorb-references-binding.ads \
		polyorb-representations.ads \
		polyorb-representations-http.ads \
		polyorb-representations-srp.ads \
		polyorb-representations-test.ads \
		polyorb-requests.ads \
		polyorb-smart_pointers.ads \
		polyorb-soft_links.ads \
		polyorb-static_dict.ads \
		polyorb-task_info.ads \
		polyorb-test_object.ads \
		polyorb-transport.ads \
		polyorb-transport-sockets.ads \
		polyorb-types.ads \
		polyorb-utils.ads \
		polyorb-utils-srp.ads \
		sequences-bounded.ads \
		sequences-unbounded.ads \
		sequences-unbounded-search.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
		polyorb.ads \
		polyorb-constants.ads \
		polyorb-filters-interface.ads \
		polyorb-object_maps.ads \
		polyorb-objects-interface.ads \
		polyorb-orb-interface.ads \
		polyorb-poa.ads \
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
		polyorb-setup.ads \
		polyorb-sockets.ads \
		polyorb-storage_pools.ads \
		polyorb-test.ads \
		sequences.ads

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

depend: all
	$(srcdir)/../support/mkdep $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
