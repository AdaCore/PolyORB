SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(libdir)/polyorb-giop

ADA_SPECS_WITH_BODY = \
	polyorb-binding_data-iiop.ads \
	polyorb-giop_p-exceptions.ads \
	polyorb-protocols-giop-giop_1_0.ads \
	polyorb-protocols-giop-giop_1_1.ads \
	polyorb-protocols-giop-giop_1_2.ads \
	polyorb-protocols-giop.ads \
	polyorb-setup-tcp_access_points-iiop.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	polyorb-giop_p.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-giop.la
libpolyorb_giop_la_SOURCES =
libpolyorb_giop_la_LIBADD = $(ADA_LIBADD)
libpolyorb_giop_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-giop.la

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
	$(INSTALL) -m 444 $(srcdir)/*.ad[bs] $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 *.ali $(DESTDIR)$(alidir)

depend-local: all
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb-binding_data-iiop.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-configuration.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters-slicers.ads ./../polyorb-filters.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_policies.ads ./../polyorb-protocols.ads ./../polyorb-references-ior.ads ./../polyorb-references.ads ./../polyorb-representations-cdr.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport-sockets.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-binding_data-iiop.adb polyorb-binding_data-iiop.ads polyorb-protocols-giop.ads 
polyorb-giop_p-exceptions.lo:./../polyorb-any.ads ./../polyorb-exceptions.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-soft_links.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-giop_p-exceptions.adb polyorb-giop_p-exceptions.ads polyorb-giop_p.ads 
polyorb-giop_p.lo:./../polyorb.ads polyorb-giop_p.ads 
polyorb-protocols-giop-giop_1_0.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_policies.ads ./../polyorb-protocols.ads ./../polyorb-references-ior.ads ./../polyorb-references.ads ./../polyorb-representations-cdr.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-binding_data-iiop.ads polyorb-protocols-giop-giop_1_0.adb polyorb-protocols-giop-giop_1_0.ads polyorb-protocols-giop.ads 
polyorb-protocols-giop-giop_1_1.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_policies.ads ./../polyorb-protocols.ads ./../polyorb-references-ior.ads ./../polyorb-references.ads ./../polyorb-representations-cdr.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-binding_data-iiop.ads polyorb-protocols-giop-giop_1_1.adb polyorb-protocols-giop-giop_1_1.ads polyorb-protocols-giop.ads 
polyorb-protocols-giop-giop_1_2.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_policies.ads ./../polyorb-protocols.ads ./../polyorb-references-ior.ads ./../polyorb-references.ads ./../polyorb-representations-cdr.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-binding_data-iiop.ads polyorb-protocols-giop-giop_1_2.adb polyorb-protocols-giop-giop_1_2.ads polyorb-protocols-giop.ads 
polyorb-protocols-giop.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data-local.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-exceptions.ads ./../polyorb-filters-interface.ads ./../polyorb-filters.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects-interface.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb-interface.ads ./../polyorb-orb.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_policies.ads ./../polyorb-protocols.ads ./../polyorb-references-ior.ads ./../polyorb-references.ads ./../polyorb-representations-cdr.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.adb ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-binding_data-iiop.ads polyorb-giop_p-exceptions.ads polyorb-giop_p.ads polyorb-protocols-giop-giop_1_0.ads polyorb-protocols-giop-giop_1_1.ads polyorb-protocols-giop-giop_1_2.ads polyorb-protocols-giop.adb polyorb-protocols-giop.ads 
polyorb-setup-tcp_access_points-iiop.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-configuration.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters-slicers.ads ./../polyorb-filters.ads ./../polyorb-initialization.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb.ads ./../polyorb-poa_policies-thread_policy.ads ./../polyorb-poa_policies.ads ./../polyorb-protocols.ads ./../polyorb-references-ior.ads ./../polyorb-references.ads ./../polyorb-representations-cdr.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-setup-tcp_access_points.ads ./../polyorb-setup.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport-sockets.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-chained_lists.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings-lists.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-binding_data-iiop.ads polyorb-protocols-giop.ads polyorb-setup-tcp_access_points-iiop.adb polyorb-setup-tcp_access_points-iiop.ads 
