SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/dsa

ADA_SPECS_WITH_BODY = \
	s-polint.ads \
	polyorb-dsa_p-exceptions.ads \
	polyorb-dsa_p-partitions.ads \
	polyorb-poa_config-racws.ads

# polyorb-dsa_p-partitions.ads is an RCI unit, needs to be compiled
# with different options for partition server and clients.

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	polyorb-dsa_p.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-dsa.la
libpolyorb_dsa_la_SOURCES =
libpolyorb_dsa_la_LIBADD = $(ADA_LIBADD)
libpolyorb_dsa_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-dsa.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. $(GNATFLAGS)

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
polyorb-dsa_p.lo:./../polyorb.ads polyorb-dsa_p.ads 
polyorb-dsa_p-exceptions.lo:./../polyorb.ads ./../polyorb-any.ads polyorb-dsa_p.ads polyorb-dsa_p-exceptions.adb polyorb-dsa_p-exceptions.ads ./../polyorb-exceptions.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.ads 
polyorb-dsa_p-partitions.lo:./../polyorb.ads polyorb-dsa_p.ads polyorb-dsa_p-partitions.adb polyorb-dsa_p-partitions.ads ./../polyorb-initialization.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-tasking.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-strings.adb ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.adb ./../polyorb-utils-strings-lists.ads 
polyorb-poa_config-racws.lo:./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any-objref.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-exceptions.ads ./../polyorb-filters.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-poa_config.ads ./../polyorb-poa_config-proxies.ads polyorb-poa_config-racws.adb polyorb-poa_config-racws.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-task_info.ads ./../polyorb-tasking.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-threads.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.adb ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.adb ./../polyorb-utils-strings-lists.ads s-polint.ads 
s-polint.lo:./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.adb ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any-objref.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads polyorb-dsa_p.ads polyorb-dsa_p-exceptions.ads polyorb-dsa_p-partitions.ads ./../polyorb-dynamic_dict.adb ./../polyorb-dynamic_dict.ads ./../polyorb-exceptions.ads ./../polyorb-filters.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-object_maps.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-parameters.ads ./../polyorb-poa.ads ./../polyorb-poa-basic_poa.ads ./../polyorb-poa_config.adb ./../polyorb-poa_config.ads ./../polyorb-poa_config-minimum.ads ./../polyorb-poa_manager.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-id_assignment_policy.ads ./../polyorb-poa_policies-id_uniqueness_policy.ads ./../polyorb-poa_policies-implicit_activation_policy.ads ./../polyorb-poa_policies-lifespan_policy.ads ./../polyorb-poa_policies-request_processing_policy.ads ./../polyorb-poa_policies-servant_retention_policy.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_types.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-scheduler.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences-unbounded-helper.adb ./../polyorb-sequences-unbounded-helper.ads ./../polyorb-servants.ads ./../polyorb-servants-interface.ads ./../polyorb-services.ads ./../polyorb-services-naming.ads ./../polyorb-services-naming-helper.ads ./../polyorb-services-naming-namingcontext.ads ./../polyorb-services-naming-namingcontext-client.ads ./../polyorb-setup.ads ./../polyorb-setup-proxies_poa.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-task_info.ads ./../polyorb-tasking.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-threads.ads ./../polyorb-transport.ads ./../polyorb-types.adb ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-dynamic_tables.adb ./../polyorb-utils-dynamic_tables.ads ./../polyorb-utils-hfunctions.ads ./../polyorb-utils-hfunctions-mul.adb ./../polyorb-utils-hfunctions-mul.ads ./../polyorb-utils-htables.ads ./../polyorb-utils-htables-perfect.adb ./../polyorb-utils-htables-perfect.ads ./../polyorb-utils-simple_flags.adb ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.adb ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.adb ./../polyorb-utils-strings-lists.ads s-polint.adb s-polint.ads 
