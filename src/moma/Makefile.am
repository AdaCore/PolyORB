SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/moma

ADA_SPECS_WITH_BODY = \
	moma-configuration-server.ads \
	moma-configuration.ads \
	moma-connection_factories.ads \
	moma-connections.ads \
	moma-destinations.ads \
	moma-message_consumers.ads \
	moma-message_handlers.ads \
	moma-message_producers.ads \
	moma-messages-manys.ads \
	moma-messages-mbytes.ads \
	moma-messages-mexecutes.ads \
	moma-messages-mmaps.ads \
	moma-messages-mtexts.ads \
	moma-messages.ads \
	moma-sessions.ads \
	moma-types.ads \
	polyorb-moma_p-exceptions.ads \
	polyorb-moma_p-provider-message_consumer.ads \
	polyorb-moma_p-provider-message_handler.ads \
	polyorb-moma_p-provider-message_pool.ads \
	polyorb-moma_p-provider-message_producer.ads \
	polyorb-moma_p-provider-routers.ads \
	polyorb-moma_p-provider-topic_datas.ads \
	polyorb-moma_p-provider-warehouse.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	moma.ads \
	polyorb-moma_p.ads \
	polyorb-moma_p-provider.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-moma.la
libpolyorb_moma_la_SOURCES =
libpolyorb_moma_la_LIBADD = $(ADA_LIBADD) 
libpolyorb_moma_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-moma.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. \
	$(XMLADA_INCS) $(GNATFLAGS)

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
