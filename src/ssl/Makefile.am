SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb

ADA_SPECS_WITH_BODY = \
	polyorb-asynch_ev-sockets-ssl.ads \
	polyorb-ssl.ads \
	polyorb-transport-connected-sockets-ssl.ads \
	polyorb-utils-ssl_access_points.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo) polyorb_ssl.lo
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-ssl.la

libpolyorb_ssl_la_SOURCES =
libpolyorb_ssl_la_LIBADD = $(ADA_LIBADD)
libpolyorb_ssl_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

ADAFLAGS+= $(STD_ADAFLAGS) \
	-I. -I$(srcdir) -I.. -I$(srcdir)/..

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
polyorb-asynch_ev-sockets-ssl.lo: $(top_srcdir)/src/polyorb-annotations.ads $(top_srcdir)/src/polyorb-asynch_ev-sockets.ads $(top_srcdir)/src/polyorb-asynch_ev.ads $(top_srcdir)/src/polyorb-initialization.ads $(top_srcdir)/src/polyorb-jobs.ads $(top_srcdir)/src/polyorb-log.adb $(top_srcdir)/src/polyorb-log.ads $(top_srcdir)/src/polyorb-sockets.ads $(top_srcdir)/src/polyorb-sockets_initialization.ads $(top_srcdir)/src/polyorb-utils-chained_lists.adb $(top_srcdir)/src/polyorb-utils-chained_lists.ads $(top_srcdir)/src/polyorb-utils-strings-lists.ads $(top_srcdir)/src/polyorb-utils-strings.ads $(top_srcdir)/src/polyorb-utils.ads $(top_srcdir)/src/polyorb.ads $(top_srcdir)/src/ssl/polyorb-asynch_ev-sockets-ssl.adb $(top_srcdir)/src/ssl/polyorb-asynch_ev-sockets-ssl.ads $(top_srcdir)/src/ssl/polyorb-ssl.ads
polyorb-ssl.lo: $(top_srcdir)/src/polyorb-initialization.ads $(top_srcdir)/src/polyorb-platform-ssl_linker_options.ads $(top_srcdir)/src/polyorb-platform.ads $(top_srcdir)/src/polyorb-sockets.ads $(top_srcdir)/src/polyorb-sockets_initialization.ads $(top_srcdir)/src/polyorb-utils-chained_lists.adb $(top_srcdir)/src/polyorb-utils-chained_lists.ads $(top_srcdir)/src/polyorb-utils-strings-lists.adb $(top_srcdir)/src/polyorb-utils-strings-lists.ads $(top_srcdir)/src/polyorb-utils-strings.adb $(top_srcdir)/src/polyorb-utils-strings.ads $(top_srcdir)/src/polyorb-utils.ads $(top_srcdir)/src/polyorb.ads $(top_srcdir)/src/ssl/polyorb-ssl.adb $(top_srcdir)/src/ssl/polyorb-ssl.ads
polyorb-transport-connected-sockets-ssl.lo: $(top_srcdir)/src/polyorb-annotations.ads $(top_srcdir)/src/polyorb-any-exceptionlist.ads $(top_srcdir)/src/polyorb-any-nvlist.ads $(top_srcdir)/src/polyorb-any.ads $(top_srcdir)/src/polyorb-asynch_ev-sockets.ads $(top_srcdir)/src/polyorb-asynch_ev.adb $(top_srcdir)/src/polyorb-asynch_ev.ads $(top_srcdir)/src/polyorb-binding_data.ads $(top_srcdir)/src/polyorb-binding_objects.ads $(top_srcdir)/src/polyorb-buffers.adb $(top_srcdir)/src/polyorb-buffers.ads $(top_srcdir)/src/polyorb-components.ads $(top_srcdir)/src/polyorb-errors.ads $(top_srcdir)/src/polyorb-filters.ads $(top_srcdir)/src/polyorb-initialization.ads $(top_srcdir)/src/polyorb-jobs.ads $(top_srcdir)/src/polyorb-log.adb $(top_srcdir)/src/polyorb-log.ads $(top_srcdir)/src/polyorb-obj_adapters.ads $(top_srcdir)/src/polyorb-objects.ads $(top_srcdir)/src/polyorb-opaque-chunk_pools.adb $(top_srcdir)/src/polyorb-opaque-chunk_pools.ads $(top_srcdir)/src/polyorb-opaque.ads $(top_srcdir)/src/polyorb-orb.ads $(top_srcdir)/src/polyorb-orb_controller.ads $(top_srcdir)/src/polyorb-qos.ads $(top_srcdir)/src/polyorb-references.ads $(top_srcdir)/src/polyorb-request_scheduler.ads $(top_srcdir)/src/polyorb-requests.ads $(top_srcdir)/src/polyorb-servants.ads $(top_srcdir)/src/polyorb-smart_pointers.ads $(top_srcdir)/src/polyorb-sockets.ads $(top_srcdir)/src/polyorb-sockets_initialization.ads $(top_srcdir)/src/polyorb-task_info.ads $(top_srcdir)/src/polyorb-tasking-condition_variables.ads $(top_srcdir)/src/polyorb-tasking-idle_tasks_managers.ads $(top_srcdir)/src/polyorb-tasking-mutexes.ads $(top_srcdir)/src/polyorb-tasking-threads.ads $(top_srcdir)/src/polyorb-tasking.ads $(top_srcdir)/src/polyorb-transport-connected-sockets.ads $(top_srcdir)/src/polyorb-transport-connected.ads $(top_srcdir)/src/polyorb-transport-handlers.ads $(top_srcdir)/src/polyorb-transport.ads $(top_srcdir)/src/polyorb-types.ads $(top_srcdir)/src/polyorb-utils-chained_lists.adb $(top_srcdir)/src/polyorb-utils-chained_lists.ads $(top_srcdir)/src/polyorb-utils-simple_flags.adb $(top_srcdir)/src/polyorb-utils-simple_flags.ads $(top_srcdir)/src/polyorb-utils-strings-lists.ads $(top_srcdir)/src/polyorb-utils-strings.ads $(top_srcdir)/src/polyorb-utils.ads $(top_srcdir)/src/polyorb.ads $(top_srcdir)/src/ssl/polyorb-asynch_ev-sockets-ssl.ads $(top_srcdir)/src/ssl/polyorb-ssl.ads $(top_srcdir)/src/ssl/polyorb-transport-connected-sockets-ssl.adb $(top_srcdir)/src/ssl/polyorb-transport-connected-sockets-ssl.ads
polyorb-utils-ssl_access_points.lo: $(top_srcdir)/src/polyorb-annotations.ads $(top_srcdir)/src/polyorb-any-exceptionlist.ads $(top_srcdir)/src/polyorb-any-nvlist.ads $(top_srcdir)/src/polyorb-any.ads $(top_srcdir)/src/polyorb-asynch_ev.ads $(top_srcdir)/src/polyorb-binding_data.ads $(top_srcdir)/src/polyorb-binding_objects.ads $(top_srcdir)/src/polyorb-buffers.ads $(top_srcdir)/src/polyorb-components.ads $(top_srcdir)/src/polyorb-errors.ads $(top_srcdir)/src/polyorb-filters.ads $(top_srcdir)/src/polyorb-initialization.ads $(top_srcdir)/src/polyorb-jobs.ads $(top_srcdir)/src/polyorb-log.adb $(top_srcdir)/src/polyorb-log.ads $(top_srcdir)/src/polyorb-obj_adapters.ads $(top_srcdir)/src/polyorb-objects.ads $(top_srcdir)/src/polyorb-opaque-chunk_pools.adb $(top_srcdir)/src/polyorb-opaque-chunk_pools.ads $(top_srcdir)/src/polyorb-opaque.ads $(top_srcdir)/src/polyorb-orb.ads $(top_srcdir)/src/polyorb-orb_controller.ads $(top_srcdir)/src/polyorb-qos.ads $(top_srcdir)/src/polyorb-references.ads $(top_srcdir)/src/polyorb-request_scheduler.ads $(top_srcdir)/src/polyorb-requests.ads $(top_srcdir)/src/polyorb-servants.ads $(top_srcdir)/src/polyorb-setup.ads $(top_srcdir)/src/polyorb-smart_pointers.ads $(top_srcdir)/src/polyorb-sockets.ads $(top_srcdir)/src/polyorb-sockets_initialization.ads $(top_srcdir)/src/polyorb-task_info.ads $(top_srcdir)/src/polyorb-tasking-condition_variables.ads $(top_srcdir)/src/polyorb-tasking-idle_tasks_managers.ads $(top_srcdir)/src/polyorb-tasking-mutexes.ads $(top_srcdir)/src/polyorb-tasking-threads.ads $(top_srcdir)/src/polyorb-tasking.ads $(top_srcdir)/src/polyorb-transport-connected-sockets.ads $(top_srcdir)/src/polyorb-transport-connected.ads $(top_srcdir)/src/polyorb-transport-handlers.ads $(top_srcdir)/src/polyorb-transport.ads $(top_srcdir)/src/polyorb-types.ads $(top_srcdir)/src/polyorb-utils-chained_lists.adb $(top_srcdir)/src/polyorb-utils-chained_lists.ads $(top_srcdir)/src/polyorb-utils-simple_flags.adb $(top_srcdir)/src/polyorb-utils-simple_flags.ads $(top_srcdir)/src/polyorb-utils-strings-lists.ads $(top_srcdir)/src/polyorb-utils-strings.ads $(top_srcdir)/src/polyorb-utils.ads $(top_srcdir)/src/polyorb.ads $(top_srcdir)/src/ssl/polyorb-ssl.ads $(top_srcdir)/src/ssl/polyorb-transport-connected-sockets-ssl.ads $(top_srcdir)/src/ssl/polyorb-utils-ssl_access_points.adb $(top_srcdir)/src/ssl/polyorb-utils-ssl_access_points.ads
