SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/srp

ADA_SPECS_WITH_BODY = \
	polyorb-binding_data-srp.ads \
	polyorb-protocols-srp.ads \
	polyorb-representations-srp.ads \
	polyorb-setup-access_points-srp.ads \
	polyorb-utils-srp.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-srp.la
libpolyorb_srp_la_SOURCES =
libpolyorb_srp_la_LIBADD = $(ADA_LIBADD) 
libpolyorb_srp_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-srp.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. \
	$(GNATFLAGS)

ADACOMPILER = ../../support/adacompiler

SUFFIXES = .adb .ads .lo .ali .c

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
	for f in $(srcdir)/*.ad[bs] *.ali; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done

depend-local: all
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb-binding_data-srp.lo:./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads polyorb-binding_data-srp.adb polyorb-binding_data-srp.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-exceptions.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-srp.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-scheduler.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-setup.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-task_info.ads ./../polyorb-tasking.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-threads.ads ./../polyorb-transport.ads ./../polyorb-transport-connected.ads ./../polyorb-transport-connected-sockets.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.adb ./../polyorb-utils-simple_flags.ads polyorb-utils-srp.ads ./../polyorb-utils-strings.ads 
polyorb-protocols-srp.lo:./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-binding_data-local.ads ./../polyorb-buffers.ads ./../polyorb-components.adb ./../polyorb-components.ads ./../polyorb-exceptions.ads ./../polyorb-filters.ads ./../polyorb-filters-interface.ads ./../polyorb-jobs.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.adb ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-orb-interface.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-srp.adb polyorb-protocols-srp.ads ./../polyorb-references.ads ./../polyorb-representations.ads polyorb-representations-srp.ads ./../polyorb-requests.ads ./../polyorb-scheduler.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-task_info.ads ./../polyorb-tasking.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-threads.ads ./../polyorb-transport.ads ./../polyorb-types.adb ./../polyorb-types.ads ./../polyorb-utils.adb ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.adb ./../polyorb-utils-simple_flags.ads polyorb-utils-srp.ads ./../polyorb-utils-strings.ads 
polyorb-representations-srp.lo:./../polyorb.ads ./../polyorb-any.adb ./../polyorb-any.ads ./../polyorb-buffers.adb ./../polyorb-buffers.ads ./../polyorb-initialization.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-objects.adb ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-representations.ads polyorb-representations-srp.adb polyorb-representations-srp.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-types.adb ./../polyorb-types.ads ./../polyorb-utils.adb ./../polyorb-utils.ads ./../polyorb-utils-buffers.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads polyorb-utils-srp.ads ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.ads 
polyorb-setup-access_points-srp.lo:./../polyorb.ads ./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads polyorb-binding_data-srp.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-exceptions.ads ./../polyorb-filters.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque.ads ./../polyorb-opaque-chunk_pools.adb ./../polyorb-opaque-chunk_pools.ads ./../polyorb-orb.ads ./../polyorb-parameters.ads ./../polyorb-poa_policies.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-protocols.ads polyorb-protocols-srp.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-scheduler.ads ./../polyorb-sequences.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-servants.ads ./../polyorb-setup-access_points.ads polyorb-setup-access_points-srp.adb polyorb-setup-access_points-srp.ads ./../polyorb-setup.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-sockets_initialization.ads ./../polyorb-task_info.ads ./../polyorb-tasking.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-threads.ads ./../polyorb-transport.ads ./../polyorb-transport-connected.ads ./../polyorb-transport-connected-sockets.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb-utils-chained_lists.adb ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.adb ./../polyorb-utils-simple_flags.ads polyorb-utils-srp.ads ./../polyorb-utils-strings.adb ./../polyorb-utils-strings.ads ./../polyorb-utils-strings-lists.adb ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-tcp_access_points.ads 
polyorb-utils-srp.lo:./../polyorb.ads ./../polyorb-any.ads ./../polyorb-objects.adb ./../polyorb-objects.ads ./../polyorb-smart_pointers.ads ./../polyorb-types.adb ./../polyorb-types.ads ./../polyorb-utils.ads polyorb-utils-srp.adb polyorb-utils-srp.ads 
