SUBDIRS = .

alidir = $(libdir)/polyorb-soap

ADA_SPECS_WITH_BODY = \
	polyorb-binding_data-soap.ads \
	polyorb-filters-http.ads \
	polyorb-protocols-http.ads \
	polyorb-protocols-soap.ads \
	polyorb-representations-http.ads \
	polyorb-representations-soap-any.ads \
	polyorb-representations-soap-name_spaces.ads \
	polyorb-representations-soap.ads \
	polyorb-utils-http.ads \
	polyorb-utils-http_messages.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-soap.la
libpolyorb_soap_la_SOURCES =
libpolyorb_soap_la_LIBADD = $(ADA_LIBADD)
libpolyorb_soap_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-soap.la

ADAFLAGS = $(GNATFLAGS) \
	-I. -I$(srcdir) -I.. -I$(srcdir)/..

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
polyorb-binding_data-soap.lo:./../polyorb-annotations.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb-interface.ads ./../polyorb-orb.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport-sockets.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads polyorb-binding_data-soap.adb polyorb-binding_data-soap.ads polyorb-filters-http.ads polyorb-protocols-http.ads polyorb-protocols-soap.ads polyorb-representations-soap.ads polyorb-utils-http.ads polyorb-utils-http_messages.ads 
polyorb-filters-http.lo:./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters-interface.ads ./../polyorb-filters.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-sockets.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads polyorb-filters-http.adb polyorb-filters-http.ads 
polyorb-protocols-http.lo:./../polyorb-annotations.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb-interface.ads ./../polyorb-orb.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-representations-test.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads polyorb-protocols-http.adb polyorb-protocols-http.ads polyorb-utils-http.ads polyorb-utils-http_messages.ads 
polyorb-protocols-soap.lo:./../polyorb-annotations.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data-local.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-filters-interface.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb-interface.ads ./../polyorb-orb.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads polyorb-binding_data-soap.ads polyorb-protocols-http.ads polyorb-protocols-soap.adb polyorb-protocols-soap.ads polyorb-representations-soap-any.ads polyorb-representations-soap.ads polyorb-utils-http.ads polyorb-utils-http_messages.ads 
polyorb-representations-http.lo:./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-locks.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-representations.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads polyorb-representations-http.adb polyorb-representations-http.ads 
polyorb-representations-soap-any.lo:./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-locks.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-representations.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads polyorb-representations-soap-any.adb polyorb-representations-soap-any.ads polyorb-representations-soap.ads 
polyorb-representations-soap-name_spaces.lo:./../polyorb-annotations.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters-simple.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb-interface.ads ./../polyorb-orb.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-representations.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.adb ./../sequences-unbounded.ads ./../sequences.ads polyorb-protocols-http.ads polyorb-protocols-soap.ads polyorb-representations-soap-name_spaces.adb polyorb-representations-soap-name_spaces.ads polyorb-representations-soap.ads polyorb-utils-http_messages.ads 
polyorb-representations-soap.lo:./../polyorb-annotations.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-dynamic_dict.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-representations.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb.ads ./../sequences-unbounded.adb ./../sequences-unbounded.ads ./../sequences.ads polyorb-representations-soap.adb polyorb-representations-soap.ads 
polyorb-utils-http.lo:./../polyorb-utils.ads ./../polyorb.ads polyorb-utils-http.adb polyorb-utils-http.ads 
polyorb-utils-http_messages.lo:./../polyorb-annotations.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-filters.ads ./../polyorb-jobs.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-orb-interface.ads ./../polyorb-orb.ads ./../polyorb-protocols.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils.ads ./../polyorb.ads ./../sequences-unbounded.ads ./../sequences.ads polyorb-protocols-http.ads polyorb-utils-http_messages.adb polyorb-utils-http_messages.ads 
