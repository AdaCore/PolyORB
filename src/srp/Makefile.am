SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(libdir)/polyorb-srp


ADA_SPECS_WITH_BODY = \
	polyorb-binding_data-srp.ads \
	polyorb-protocols-srp.ads \
	polyorb-representations-srp.ads \
	polyorb-setup-tcp_access_points-srp.ads \
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
	$(INSTALL) -m 444 $(srcdir)/*.ad[bs] $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 *.ali $(DESTDIR)$(alidir)

depend-local: all
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb-binding_data-srp.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_policies.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-tasking-advanced_mutexes.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-rw_locks.ads ./../polyorb-tasking-soft_links.ads ./../polyorb-tasking-threads.ads ./../polyorb-tasking-watchers.ads ./../polyorb-tasking.ads ./../polyorb-transport-sockets.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-binding_data-srp.adb polyorb-binding_data-srp.ads polyorb-protocols-srp.ads polyorb-utils-srp.ads 
polyorb-protocols-srp.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data-local.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters-interface.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb-interface.ads ./../polyorb-orb.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_policies.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-tasking-advanced_mutexes.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-rw_locks.ads ./../polyorb-tasking-soft_links.ads ./../polyorb-tasking-threads.ads ./../polyorb-tasking-watchers.ads ./../polyorb-tasking.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-protocols-srp.adb polyorb-protocols-srp.ads polyorb-representations-srp.ads polyorb-utils-srp.ads 
polyorb-representations-srp.lo:./../polyorb-any.ads ./../polyorb-buffers.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-representations.ads ./../polyorb-sockets.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-rw_locks.ads ./../polyorb-tasking.ads ./../polyorb-types.ads ./../polyorb-utils-buffers.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-representations-srp.adb polyorb-representations-srp.ads polyorb-utils-srp.ads 
polyorb-setup-tcp_access_points-srp.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-configuration.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_policies.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-setup-tcp_access_points.ads ./../polyorb-setup.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-tasking-advanced_mutexes.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-rw_locks.ads ./../polyorb-tasking-soft_links.ads ./../polyorb-tasking-threads.ads ./../polyorb-tasking-watchers.ads ./../polyorb-tasking.ads ./../polyorb-transport-sockets.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-binding_data-srp.ads polyorb-protocols-srp.ads polyorb-setup-tcp_access_points-srp.adb polyorb-setup-tcp_access_points-srp.ads polyorb-utils-srp.ads 
polyorb-utils-srp.lo:./../polyorb-any.ads ./../polyorb-objects.ads ./../polyorb-tasking-condition_variables.ads ./../polyorb-tasking-mutexes.ads ./../polyorb-tasking-rw_locks.ads ./../polyorb-tasking.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-utils-srp.adb polyorb-utils-srp.ads 
