SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(libdir)/polyorb-moma


ADA_SPECS_WITH_BODY = \
	moma-connection_factories-queues.ads \
	moma-connection_factories-topics.ads \
	moma-connections.ads \
	moma-connections-queues.ads \
	moma-connections-topics.ads \
	moma-destinations-queues.ads \
	moma-destinations-topics.ads \
	moma-message_consumers.ads \
	moma-message_consumers-queues.ads \
	moma-message_consumers-topics.ads \
	moma-message_pool.ads \
	moma-message_pool-impl.ads \
	moma-message_pool-warehouse.ads \
	moma-message_producers.ads \
	moma-message_producers-queues.ads \
	moma-message_producers-topics.ads \
	moma-messages.ads \
	moma-messages-marrays.ads \
	moma-messages-mrecords.ads \
	moma-messages-mstreams.ads \
	moma-messages-mtexts.ads \
	moma-sessions.ads \
	moma-sessions-queues.ads \
	moma-sessions-topics.ads \
	polyorb-minimal_servant.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	moma.ads \
	moma-destinations.ads \
	moma-messages-mbytes.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-moma.la
libpolyorb_moma_la_SOURCES =
libpolyorb_moma_la_LIBADD = $(ADA_LIBADD) 
libpolyorb_moma_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-moma.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. \
	-I$(srcdir)/../corba $(XMLADA_INCLUDE) $(GNATFLAGS)
# XXX CORBA included temporarily because of dep of P.Binding_Data.SOAP on
# P.Representations.CDR.

ADACOMPILER = ../../support/adacompiler

SUFFIXES = .adb .ads .lo .ali .c

polyorb-http_methods_GPERF.c polyorb-http_methods.adb: \
  polyorb-http_methods.ads
	$(srcdir)/gen_keywords $(srcdir)/polyorb-http_methods.ads

polyorb-http_headers_GPERF.c polyorb-http_headers.adb: \
  polyorb-http_headers.ads
	$(srcdir)/gen_keywords $(srcdir)/polyorb-http_headers.ads

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
	$(srcdir)/../../support/mkdep -I$(srcdir)/.. -I$(srcdir)/../corba $(srcdir)
# XXX dep on CORBA is necessary because of P.R.IOR.

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
moma-connection_factories-queues.lo:moma-connection_factories-queues.adb moma-connection_factories-queues.ads moma-connection_factories.ads moma-connections-queues.ads moma-connections.ads moma-destinations-queues.ads moma-destinations.ads moma-message_consumers-queues.ads moma-message_consumers.ads moma-message_producers-queues.ads moma-message_producers.ads moma-messages-marrays.ads moma-messages-mbytes.ads moma-messages-mrecords.ads moma-messages-mstreams.ads moma-messages-mtexts.ads moma-messages.ads moma-sessions-queues.ads moma-sessions.ads moma.ads temp.ads 
moma-connection_factories-topics.lo:moma-connection_factories-topics.adb moma-connection_factories-topics.ads moma-connection_factories.ads moma-connections-topics.ads moma-connections.ads moma-destinations-topics.ads moma-destinations.ads moma-message_consumers-topics.ads moma-message_consumers.ads moma-message_producers-topics.ads moma-message_producers.ads moma-messages-marrays.ads moma-messages-mbytes.ads moma-messages-mrecords.ads moma-messages-mstreams.ads moma-messages-mtexts.ads moma-messages.ads moma-sessions-topics.ads moma-sessions.ads moma.ads temp.ads 
moma-connections-queues.lo:moma-connections-queues.adb moma-connections-queues.ads moma-connections.ads moma-destinations-queues.ads moma-destinations.ads moma-message_consumers-queues.ads moma-message_consumers.ads moma-message_producers-queues.ads moma-message_producers.ads moma-messages-marrays.ads moma-messages-mbytes.ads moma-messages-mrecords.ads moma-messages-mstreams.ads moma-messages-mtexts.ads moma-messages.ads moma-sessions-queues.ads moma-sessions.ads moma.ads temp.ads 
moma-connections-topics.lo:moma-connections-topics.adb moma-connections-topics.ads moma-connections.ads moma-destinations-topics.ads moma-destinations.ads moma-message_consumers-topics.ads moma-message_consumers.ads moma-message_producers-topics.ads moma-message_producers.ads moma-messages-marrays.ads moma-messages-mbytes.ads moma-messages-mrecords.ads moma-messages-mstreams.ads moma-messages-mtexts.ads moma-messages.ads moma-sessions-topics.ads moma-sessions.ads moma.ads temp.ads 
moma-connections.lo:moma-connections.adb moma-connections.ads moma-destinations.ads moma-message_consumers.ads moma-messages-marrays.ads moma-messages-mbytes.ads moma-messages-mrecords.ads moma-messages-mstreams.ads moma-messages-mtexts.ads moma-messages.ads moma-sessions.ads moma.ads temp.ads 
moma-destinations-queues.lo:moma-destinations-queues.adb moma-destinations-queues.ads moma-destinations.ads moma.ads 
moma-destinations-topics.lo:moma-destinations-topics.adb moma-destinations-topics.ads moma-destinations.ads moma.ads 
moma-destinations.lo:moma-destinations.ads moma.ads 
moma-message_consumers-queues.lo:moma-destinations-queues.ads moma-destinations.ads moma-message_consumers-queues.adb moma-message_consumers-queues.ads moma-message_consumers.ads moma-messages.ads moma.ads temp.ads 
moma-message_consumers-topics.lo:moma-destinations-topics.ads moma-destinations.ads moma-message_consumers-topics.adb moma-message_consumers-topics.ads moma-message_consumers.ads moma-messages.ads moma.ads temp.ads 
moma-message_consumers.lo:moma-destinations.ads moma-message_consumers.adb moma-message_consumers.ads moma-messages.ads moma.ads temp.ads 
moma-message_pool-impl.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters-simple.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-dynamic_tables.ads ./../polyorb-utils-htables-perfect.ads ./../polyorb-utils-htables.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads moma-message_pool-impl.adb moma-message_pool-impl.ads moma-message_pool-warehouse.ads moma-message_pool.ads moma.ads polyorb-minimal_servant.ads 
moma-message_pool-warehouse.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-locks.ads ./../polyorb-obj_adapters-simple.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-dynamic_tables.adb ./../polyorb-utils-dynamic_tables.ads ./../polyorb-utils-htables-perfect.adb ./../polyorb-utils-htables-perfect.ads ./../polyorb-utils-htables.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads moma-message_pool-warehouse.adb moma-message_pool-warehouse.ads moma-message_pool.ads moma.ads polyorb-minimal_servant.ads 
moma-message_pool.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-obj_adapters-simple.ads ./../polyorb-obj_adapters.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads moma-message_pool-impl.ads moma-message_pool.adb moma-message_pool.ads moma.ads polyorb-minimal_servant.ads 
moma-message_producers-queues.lo:moma-destinations-queues.ads moma-destinations.ads moma-message_producers-queues.adb moma-message_producers-queues.ads moma-message_producers.ads moma-messages.ads moma.ads temp.ads 
moma-message_producers-topics.lo:moma-destinations-topics.ads moma-destinations.ads moma-message_producers-topics.adb moma-message_producers-topics.ads moma-message_producers.ads moma-messages.ads moma.ads temp.ads 
moma-message_producers.lo:moma-message_producers.adb moma-message_producers.ads moma.ads temp.ads 
moma-messages-marrays.lo:moma-destinations.ads moma-messages-marrays.adb moma-messages-marrays.ads moma-messages.ads moma.ads temp.ads 
moma-messages-mbytes.lo:moma-destinations.ads moma-messages-mbytes.ads moma-messages.ads moma.ads temp.ads 
moma-messages-mrecords.lo:moma-destinations.ads moma-messages-mrecords.adb moma-messages-mrecords.ads moma-messages.ads moma.ads temp.ads 
moma-messages-mstreams.lo:moma-destinations.ads moma-messages-mstreams.adb moma-messages-mstreams.ads moma-messages.ads moma.ads temp.ads 
moma-messages-mtexts.lo:moma-destinations.ads moma-messages-mtexts.adb moma-messages-mtexts.ads moma-messages.ads moma.ads temp.ads 
moma-messages.lo:moma-destinations.ads moma-messages.adb moma-messages.ads moma.ads temp.ads 
moma-sessions-queues.lo:moma-destinations-queues.ads moma-destinations.ads moma-message_consumers-queues.ads moma-message_consumers.ads moma-message_producers-queues.ads moma-message_producers.ads moma-messages-marrays.ads moma-messages-mbytes.ads moma-messages-mrecords.ads moma-messages-mstreams.ads moma-messages-mtexts.ads moma-messages.ads moma-sessions-queues.adb moma-sessions-queues.ads moma-sessions.ads moma.ads temp.ads 
moma-sessions-topics.lo:moma-destinations-topics.ads moma-destinations.ads moma-message_consumers-topics.ads moma-message_consumers.ads moma-message_producers-topics.ads moma-message_producers.ads moma-messages-marrays.ads moma-messages-mbytes.ads moma-messages-mrecords.ads moma-messages-mstreams.ads moma-messages-mtexts.ads moma-messages.ads moma-sessions-topics.adb moma-sessions-topics.ads moma-sessions.ads moma.ads temp.ads 
moma-sessions.lo:moma-destinations.ads moma-messages-marrays.ads moma-messages-mbytes.ads moma-messages-mrecords.ads moma-messages-mstreams.ads moma-messages-mtexts.ads moma-messages.ads moma-sessions.adb moma-sessions.ads moma.ads temp.ads 
moma.lo:moma.ads 
polyorb-minimal_servant.lo:./../polyorb-annotations.ads ./../polyorb-any-exceptionlist.ads ./../polyorb-any-nvlist.ads ./../polyorb-any.ads ./../polyorb-asynch_ev.ads ./../polyorb-binding_data.ads ./../polyorb-buffers.ads ./../polyorb-components.ads ./../polyorb-locks.ads ./../polyorb-log.adb ./../polyorb-log.ads ./../polyorb-objects-interface.ads ./../polyorb-objects.ads ./../polyorb-opaque-chunk_pools.ads ./../polyorb-opaque.ads ./../polyorb-references.ads ./../polyorb-requests.ads ./../polyorb-sequences-unbounded.ads ./../polyorb-sequences.ads ./../polyorb-servants.ads ./../polyorb-smart_pointers.ads ./../polyorb-sockets.ads ./../polyorb-soft_links.ads ./../polyorb-storage_pools.ads ./../polyorb-task_info.ads ./../polyorb-transport.ads ./../polyorb-types.ads ./../polyorb-utils-simple_flags.ads ./../polyorb-utils-strings.ads ./../polyorb-utils.ads ./../polyorb.ads polyorb-minimal_servant.adb polyorb-minimal_servant.ads 
