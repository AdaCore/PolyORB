AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../support

ADA_FULL = \
	   broca-debug.ads \
	   broca-opaque-chunk_pools.ads \
	   broca-buffers.ads \
	   broca-buffers-io_operations.ads
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

ADA_OBJS = $(ADA_SPECS:.ads=.o)

lib_LTLIBRARIES = libbroca.la
libbroca_la_SOURCES = $(ADA_SPECS) $(ADA_FULL:.ads=.adb)
libbroca_la_LIBADD = $(ADA_FULL:.ads=.lo)
libbroca_la_LDFLAGS = -lgnat -no-undefined -release 0.9.0

CLEANFILES = *.ali

LINK = libtool $(ADA) -o libbroca.la

ADAFLAGS = -gnatwu -gnatwl -gnatf -gnatgao -I../adasockets/src -g -O2 $(CFLAGS)

%.lo: %.adb
	@chmod +x ./adacompiler
	$(LIBTOOL) --mode=compile ./adacompiler -c $(ADAFLAGS) $<

%.lo: %.ads
	@chmod +x ./adacompiler
	$(LIBTOOL) --mode=compile ./adacompiler -c $(ADAFLAGS) $<

