SUBDIRS = .

include $(srcdir)/../../Makefile.common

alidir = $(libdir)/polyorb-iiop

ADA_SPECS_WITH_BODY = \
	polyorb-binding_data-iiop.ads \
	polyorb-setup-iiop.ads \
	polyorb-setup-tcp_access_points-iiop.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) 

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-iiop.la
libpolyorb_iiop_la_SOURCES =
libpolyorb_iiop_la_LIBADD = $(ADA_LIBADD)
libpolyorb_iiop_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-iiop.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. -I../../ -I$(srcdir)/../.. $(GNATFLAGS)

ADACOMPILER = ../../../support/adacompiler

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

depend-local-local: all
	$(srcdir)/../../../support/mkdep -I$(srcdir)/.. -I$(srcdir)/../.. $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb-binding_data-iiop.lo:./../../polyorb.ads ./../../polyorb-annotations.ads ./../../polyorb-any.ads ./../../polyorb-any-exceptionlist.ads ./../../polyorb-any-nvlist.ads ./../../polyorb-asynch_ev.ads ./../../polyorb-binding_data.ads polyorb-binding_data-iiop.adb polyorb-binding_data-iiop.ads ./../../polyorb-buffers.ads ./../../polyorb-components.ads ./../../polyorb-configuration.ads ./../../polyorb-exceptions.ads ./../../polyorb-filters.ads ./../../polyorb-filters-interface.ads ./../../polyorb-filters-slicers.ads ./../polyorb-giop_p.ads ./../polyorb-giop_p-tagged_components.ads ./../../polyorb-initialization.ads ./../../polyorb-jobs.ads ./../../polyorb-log.adb ./../../polyorb-log.ads ./../../polyorb-obj_adapters.ads ./../../polyorb-objects.ads ./../../polyorb-opaque.ads ./../../polyorb-opaque-chunk_pools.ads ./../../polyorb-orb.ads ./../../polyorb-poa_policies.ads ./../../polyorb-poa_policies-thread_policy.ads ./../../polyorb-protocols.ads ./../polyorb-protocols-giop.ads ./../../polyorb-references.ads ./../../polyorb-references-ior.ads ./../../polyorb-representations.ads ./../../polyorb-representations-cdr.ads ./../../polyorb-requests.ads ./../../polyorb-scheduler.ads ./../../polyorb-sequences.ads ./../../polyorb-sequences-unbounded.ads ./../../polyorb-servants.ads ./../../polyorb-smart_pointers.ads ./../../polyorb-sockets.ads ./../../polyorb-task_info.ads ./../../polyorb-tasking.ads ./../../polyorb-tasking-condition_variables.ads ./../../polyorb-tasking-mutexes.ads ./../../polyorb-tasking-threads.ads ./../../polyorb-transport.ads ./../../polyorb-transport-connected.ads ./../../polyorb-transport-connected-sockets.ads ./../../polyorb-types.ads ./../../polyorb-utils.ads ./../../polyorb-utils-chained_lists.ads ./../../polyorb-utils-simple_flags.ads ./../../polyorb-utils-sockets.ads ./../../polyorb-utils-strings.ads ./../../polyorb-utils-strings-lists.ads 
polyorb-setup-iiop.lo:./../../polyorb.ads ./../../polyorb-annotations.ads ./../../polyorb-any.ads ./../../polyorb-any-exceptionlist.ads ./../../polyorb-any-nvlist.ads ./../../polyorb-asynch_ev.ads ./../../polyorb-binding_data.ads ./../../polyorb-buffers.ads ./../../polyorb-components.ads ./../../polyorb-exceptions.ads ./../../polyorb-filters.ads ./../../polyorb-filters-interface.ads ./../../polyorb-jobs.ads ./../../polyorb-obj_adapters.ads ./../../polyorb-objects.ads ./../../polyorb-opaque.ads ./../../polyorb-opaque-chunk_pools.ads ./../../polyorb-orb.ads ./../../polyorb-poa_policies.ads ./../../polyorb-poa_policies-thread_policy.ads ./../../polyorb-protocols.ads ./../polyorb-protocols-giop.ads ./../polyorb-protocols-giop-common.ads ./../polyorb-protocols-giop-giop_1_0.ads ./../polyorb-protocols-giop-giop_1_1.ads ./../polyorb-protocols-giop-giop_1_2.ads ./../../polyorb-references.ads ./../../polyorb-references-ior.ads ./../../polyorb-requests.ads ./../../polyorb-scheduler.ads ./../../polyorb-sequences.ads ./../../polyorb-sequences-unbounded.ads ./../../polyorb-servants.ads ./../../polyorb-setup.ads polyorb-setup-iiop.adb polyorb-setup-iiop.ads ./../../polyorb-smart_pointers.ads ./../../polyorb-sockets.ads ./../../polyorb-task_info.ads ./../../polyorb-tasking.ads ./../../polyorb-tasking-condition_variables.ads ./../../polyorb-tasking-mutexes.ads ./../../polyorb-tasking-threads.ads ./../../polyorb-transport.ads ./../../polyorb-types.ads ./../../polyorb-utils.ads ./../../polyorb-utils-chained_lists.ads ./../../polyorb-utils-simple_flags.ads ./../../polyorb-utils-strings.ads 
polyorb-setup-tcp_access_points-iiop.lo:./../../polyorb.ads ./../../polyorb-annotations.ads ./../../polyorb-any.ads ./../../polyorb-any-exceptionlist.ads ./../../polyorb-any-nvlist.ads ./../../polyorb-asynch_ev.ads ./../../polyorb-binding_data.ads polyorb-binding_data-iiop.ads ./../../polyorb-buffers.ads ./../../polyorb-components.ads ./../../polyorb-configuration.ads ./../../polyorb-exceptions.ads ./../../polyorb-filters.ads ./../../polyorb-filters-interface.ads ./../../polyorb-filters-slicers.ads ./../polyorb-giop_p.ads ./../polyorb-giop_p-tagged_components.ads ./../../polyorb-initialization.ads ./../../polyorb-jobs.ads ./../../polyorb-obj_adapters.ads ./../../polyorb-objects.ads ./../../polyorb-opaque.ads ./../../polyorb-opaque-chunk_pools.ads ./../../polyorb-orb.ads ./../../polyorb-poa_policies.ads ./../../polyorb-poa_policies-thread_policy.ads ./../../polyorb-protocols.ads ./../polyorb-protocols-giop.ads ./../../polyorb-references.ads ./../../polyorb-requests.ads ./../../polyorb-scheduler.ads ./../../polyorb-sequences.ads ./../../polyorb-sequences-unbounded.ads ./../../polyorb-servants.ads ./../../polyorb-setup.ads ./../../polyorb-setup-tcp_access_points.ads polyorb-setup-tcp_access_points-iiop.adb polyorb-setup-tcp_access_points-iiop.ads ./../../polyorb-smart_pointers.ads ./../../polyorb-sockets.ads ./../../polyorb-task_info.ads ./../../polyorb-tasking.ads ./../../polyorb-tasking-condition_variables.ads ./../../polyorb-tasking-mutexes.ads ./../../polyorb-tasking-threads.ads ./../../polyorb-transport.ads ./../../polyorb-transport-connected.ads ./../../polyorb-transport-connected-sockets.ads ./../../polyorb-types.ads ./../../polyorb-utils.ads ./../../polyorb-utils-chained_lists.ads ./../../polyorb-utils-simple_flags.ads ./../../polyorb-utils-strings.ads ./../../polyorb-utils-strings-lists.ads 
