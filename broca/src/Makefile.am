alidir = $(libdir)/adabroker/adalib

ADA_FULL = \
	   broca-debug.ads \
	   broca-opaque-chunk_pools.ads \
	   broca-buffers.ads \
	   broca-buffers-io_operations.ads \
           broca-exceptions.ads \
           broca-flags.ads \
           broca-giop.ads \
           broca-iiop.ads \
           broca-inet_server.ads \
           broca-ior.ads \
           broca-locks.ads \
           broca-cdr.ads \
           broca-object.ads \
           broca-orb.ads \
           broca-poa.ads \
           broca-refs.ads \
           broca-repository.ads \
           broca-rootpoa.ads \
           broca-sequences.ads \
           broca-server.ads \
           broca-stream.ads \
           broca-vararray.ads \
           corba-forward.ads \
           corba-object.ads \
           corba-orb.ads \
           corba-sequences-unbounded.ads \
           corba.ads \
           portableserver-adapteractivator-impl.ads \
           portableserver-adapteractivator.ads \
           portableserver-poa.ads \
           portableserver-poamanager.ads \
           portableserver-servantactivator.ads \
           portableserver-servantlocator.ads \
           portableserver-servantmanager-impl.ads \
           portableserver.ads

#           broca-policy.ads \
#           corba-boa.ads \
#           corba-policy.ads \
#           corba-sequences-bounded.ads \
#           portableserver-servantretentionpolicy.ads \
#           portableserver-threadpolicy.ads

ADA_SPECS = $(ADA_FULL) \
           broca.ads \
	   broca-opaque.ads \
           broca-iop.ads \
           corba-sequences.ads \
           portableserver-servantactivator-impl.ads \
           portableserver-servantlocator-impl.ads \
           portableserver-servantmanager.ads

ADA_BODIES = $(ADA_FULL:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo) $(ADASOCKET_OBJECTS:.o=.lo)

ADASOCKET_OBJECTS = \
	sockets-constants.o \
	sockets-link.o \
	sockets-multicast.o \
	sockets-naming.o \
	sockets-thin.o \
	sockets-utils.o \
	sockets.o

lib_LTLIBRARIES = libbroca.la
libbroca_la_SOURCES = $(ADA_SPECS) $(ADA_BODIES)
libbroca_la_LIBADD = $(ADA_LIBADD)
libbroca_la_LDFLAGS = -lgnat -no-undefined -release 0.9.0

CLEANFILES = *.ali

LINK = libtool $(ADA) -o libbroca.la

ADAFLAGS = -gnatwu -gnatwl -gnatf -gnatgao \
	-I. -I$(srcdir) -I../adasockets/src -I$(srcdir)/../adasockets/src \
	-g -O2 $(CFLAGS)

%.lo: %.adb
	@chmod +x ../adacompiler
	$(LIBTOOL) --mode=compile ../adacompiler -c $(ADAFLAGS) $<

%.lo: %.ads
	@chmod +x ../adacompiler
	$(LIBTOOL) --mode=compile ../adacompiler -c $(ADAFLAGS) $<

%.lo: ../adasockets/src/%.adb
	@chmod +x ../adacompiler
	$(LIBTOOL) --mode=compile ../adacompiler -c $(ADAFLAGS) $<

%.lo: ../adasockets/src/%.ads
	@chmod +x ../adacompiler
	$(LIBTOOL) --mode=compile ../adacompiler -c $(ADAFLAGS) $<

$(ADA_LIBADD): ../adasockets/src/sockets-constants.ads

install-data-local:
	$(INSTALL) -d $(alidir)
	$(INSTALL) -m 444 *.ali $(alidir)

uninstall-local:
	-$(RM) $(alidir)/*.ali

../adasockets/src/sockets-constants.ads:
	cd ../adasockets/src && $(MAKE) sockets-constants.ads
