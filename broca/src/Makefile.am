alidir = $(libdir)/adabroker

ADA_SPECS_WITH_BODY = \
	   broca-debug.ads \
	   broca-opaque-chunk_pools.ads \
	   broca-buffers.ads \
	   broca-protected_objects.ads \
	   broca-basic_startup.ads \
	   broca-configuration.ads \
	   broca-soft_links.ads \
	   broca-buffers-io_operations.ads \
	   broca-exceptions.ads \
	   broca-exceptions-stack.ads \
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
	   broca-policy.ads \
	   broca-refs.ads \
	   broca-repository.ads \
	   broca-rootpoa.ads \
	   broca-sequences.ads \
	   broca-server.ads \
	   broca-stream.ads \
	   broca-task_attributes.ads \
	   broca-vararray.ads \
	   corba-forward.ads \
	   corba-object.ads \
	   corba-orb.ads \
	   corba-nvlist.ads \
	   corba-request.ads \
	   corba-context.ads \
	   corba-nvlist.ads \
	   corba.ads \
	   corba-abstractbase.ads \
	   corba-impl.ads \
	   corba-context.ads \
	   corba-value-forward.ads \
	   portableserver-adapteractivator-impl.ads \
	   portableserver-adapteractivator.ads \
	   portableserver-current.ads \
	   portableserver-poa.ads \
	   portableserver-poamanager.ads \
	   portableserver-servantactivator.ads \
	   portableserver-servantlocator.ads \
	   portableserver-servantmanager-impl.ads \
	   portableserver.ads \
	   sequences-bounded.ads \
	   sequences-unbounded.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	   broca.ads \
	   broca-opaque.ads \
	   broca-iop.ads \
	   corba-current.ads \
	   corba-sequences.ads \
	   corba-sequences-bounded.ads \
	   corba-sequences-unbounded.ads \
	   corba-value.ads \
	   portableserver-servantactivator-impl.ads \
	   portableserver-servantlocator-impl.ads \
	   portableserver-servantmanager.ads \
	   sequences.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo) $(ADASOCKET_OBJECTS:.o=.lo)

ADASOCKET_OBJECTS = \
	sockets-constants.o \
	sockets-link.o \
	sockets-naming.o \
	sockets-thin.o \
	sockets.o

bin_SCRIPTS = adabroker-config
lib_LTLIBRARIES = libbroca.la
libbroca_la_SOURCES = $(ADA_SPECS) $(ADA_BODIES)
libbroca_la_LIBADD = $(ADA_LIBADD)
libbroca_la_LDFLAGS = -lgnat -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libbroca.la

ADAFLAGS = $(GNATFLAGS) \
	-I. -I$(srcdir) -I../adasockets/src -I$(srcdir)/../adasockets/src

ADACOMPILER = ../support/adacompiler

%.lo: %.adb
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

%.lo: %.ads
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

%.lo: ../adasockets/src/%.adb
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

%.lo: ../adasockets/src/%.ads
	@chmod a+x $(ADACOMPILER)
	$(LIBTOOL) --mode=compile $(ADACOMPILER) -c $(ADAFLAGS) $<

$(ADA_LIBADD): ../adasockets/src/sockets-constants.ads

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 ../adasockets/src/*.ad[bs] $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 $(srcdir)/*.ad[bs] $(srcdir)/../adasockets/src/*.ad[bs] $(DESTDIR)$(alidir)
	$(INSTALL) -m 444 *.ali $(DESTDIR)$(alidir)

../adasockets/src/sockets-constants.ads:
	cd ../adasockets/src && $(MAKE) sockets-constants.ads

depend: all
	$(srcdir)/../support/mkdep $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
broca-basic_startup.lo: broca.ads broca-basic_startup.ads broca-basic_startup.adb broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-rootpoa.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-orb.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads
broca-buffers-io_operations.lo: broca.ads broca-buffers.ads broca-buffers-io_operations.ads broca-buffers-io_operations.adb broca-opaque.ads broca-opaque-chunk_pools.ads
broca-buffers.lo: broca.ads broca-buffers.ads broca-buffers.adb broca-debug.ads broca-debug.adb broca-opaque.ads broca-opaque-chunk_pools.ads broca-opaque-chunk_pools.adb corba.ads sequences.ads sequences-unbounded.ads
broca-cdr.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-cdr.adb broca-debug.ads broca-debug.adb broca-exceptions.ads broca-iop.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads
broca-configuration.lo: broca.ads broca-configuration.ads broca-configuration.adb broca-protected_objects.ads broca-soft_links.ads
broca-debug.lo: broca.ads broca-debug.ads broca-debug.adb
broca-exceptions-stack.lo: broca.ads broca-buffers.ads broca-debug.ads broca-debug.adb broca-exceptions.ads broca-exceptions-stack.ads broca-exceptions-stack.adb broca-opaque.ads broca-opaque-chunk_pools.ads corba.ads sequences.ads sequences-unbounded.ads
broca-exceptions.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-exceptions.ads broca-exceptions.adb broca-exceptions-stack.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-impl.ads corba-object.ads sequences.ads sequences-unbounded.ads
broca-flags.lo: broca.ads broca-flags.ads broca-flags.adb corba.ads sequences.ads sequences-unbounded.ads
broca-giop.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-debug.ads broca-debug.adb broca-exceptions.ads broca-giop.ads broca-giop.adb broca-iop.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads
broca-iiop.lo: broca.ads broca-buffers.ads broca-buffers-io_operations.ads broca-cdr.ads broca-debug.ads broca-debug.adb broca-exceptions.ads broca-iiop.ads broca-iiop.adb broca-iop.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads
broca-inet_server.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-debug.ads broca-debug.adb broca-exceptions.ads broca-flags.ads broca-giop.ads broca-iiop.ads broca-inet_server.ads broca-inet_server.adb broca-iop.ads broca-locks.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads broca-sequences.ads broca-server.ads broca-stream.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads
broca-iop.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-debug.ads broca-debug.adb broca-iop.ads broca-iop.adb broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads
broca-ior.lo: broca.ads broca-buffers.ads broca-exceptions.ads broca-ior.ads broca-ior.adb broca-opaque.ads broca-opaque-chunk_pools.ads corba.ads sequences.ads sequences-unbounded.ads
broca-locks.lo: broca.ads broca-locks.ads broca-locks.adb
broca-object.lo: broca.ads broca-buffers.ads broca-iop.ads broca-object.ads broca-object.adb broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads corba.ads corba-impl.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads
broca-opaque-chunk_pools.lo: broca.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-opaque-chunk_pools.adb
broca-opaque.lo: broca.ads broca-opaque.ads
broca-orb.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-debug.ads broca-debug.adb broca-exceptions.ads broca-iiop.ads broca-iop.ads broca-locks.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-orb.ads broca-orb.adb broca-poa.ads broca-refs.ads broca-repository.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-orb.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads sequences-unbounded.adb
broca-poa.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-debug.ads broca-debug.adb broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-poa.adb broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads
broca-policy.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-policy.ads broca-policy.adb broca-refs.ads corba.ads sequences.ads sequences-unbounded.ads
broca-protected_objects.lo: broca.ads broca-protected_objects.ads broca-protected_objects.adb broca-soft_links.ads
broca-refs.lo: broca.ads broca-buffers.ads broca-debug.ads broca-debug.adb broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-refs.adb corba.ads sequences.ads sequences-unbounded.ads
broca-repository.lo: broca.ads broca-buffers.ads broca-debug.ads broca-debug.adb broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-repository.ads broca-repository.adb corba.ads corba-abstractbase.ads corba-impl.ads corba-object.ads sequences.ads sequences-unbounded.ads
broca-rootpoa.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-debug.ads broca-debug.adb broca-exceptions.ads broca-flags.ads broca-inet_server.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-orb.ads broca-poa.ads broca-refs.ads broca-rootpoa.ads broca-rootpoa.adb broca-sequences.ads broca-server.ads broca-stream.ads broca-vararray.ads broca-vararray.adb corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-orb.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator.ads portableserver-servantactivator-impl.ads portableserver-servantlocator.ads portableserver-servantlocator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads
broca-sequences.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads broca-sequences.adb corba.ads corba-abstractbase.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads sequences-unbounded.adb
broca-server.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-debug.ads broca-debug.adb broca-exceptions.ads broca-flags.ads broca-giop.ads broca-iop.ads broca-locks.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-orb.ads broca-poa.ads broca-refs.ads broca-sequences.ads broca-server.ads broca-server.adb broca-stream.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-orb.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads
broca-soft_links.lo: broca.ads broca-soft_links.ads broca-soft_links.adb
broca-stream.lo: broca.ads broca-buffers.ads broca-buffers-io_operations.ads broca-debug.ads broca-debug.adb broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-stream.ads broca-stream.adb
broca-task_attributes.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-task_attributes.ads broca-task_attributes.adb corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads sequences.ads sequences-unbounded.ads
broca-vararray.lo: broca.ads broca-vararray.ads broca-vararray.adb
broca.lo: broca.ads
corba-abstractbase.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-abstractbase.adb corba-impl.ads sequences.ads sequences-unbounded.ads
corba-current.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-current.ads corba-impl.ads corba-object.ads sequences.ads sequences-unbounded.ads
corba-forward.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-forward.adb corba-impl.ads corba-object.ads sequences.ads sequences-unbounded.ads
corba-impl.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-impl.ads corba-impl.adb sequences.ads sequences-unbounded.ads
corba-nvlist.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-impl.ads corba-nvlist.ads corba-nvlist.adb corba-object.ads sequences.ads sequences-unbounded.ads
corba-object.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-ior.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-impl.ads corba-object.ads corba-object.adb sequences.ads sequences-unbounded.ads
corba-orb.lo: broca.ads broca-buffers.ads broca-debug.ads broca-debug.adb broca-ior.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-orb.ads broca-poa.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-orb.ads corba-orb.adb corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads sequences-unbounded.adb
corba-request.lo: broca.ads broca-buffers.ads broca-cdr.ads broca-giop.ads broca-iop.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-request.adb corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads
corba-sequences-bounded.lo: corba.ads corba-sequences.ads corba-sequences-bounded.ads sequences.ads sequences-bounded.ads sequences-bounded.adb sequences-unbounded.ads
corba-sequences-unbounded.lo: corba.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads sequences-unbounded.adb
corba-sequences.lo: corba.ads corba-sequences.ads sequences.ads sequences-unbounded.ads
corba-value-forward.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-impl.ads corba-value.ads corba-value-forward.ads corba-value-forward.adb sequences.ads sequences-unbounded.ads
corba-value.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-impl.ads corba-value.ads sequences.ads sequences-unbounded.ads
corba.lo: broca.ads broca-buffers.ads broca-debug.ads broca-debug.adb broca-exceptions.ads broca-opaque.ads broca-opaque-chunk_pools.ads corba.ads corba.adb sequences.ads sequences-unbounded.ads sequences-unbounded.adb
portableserver-adapteractivator-impl.lo: broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-adapteractivator-impl.ads portableserver-adapteractivator-impl.adb portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads
portableserver-adapteractivator.lo: broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-adapteractivator.adb portableserver-adapteractivator-impl.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads
portableserver-current.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-task_attributes.ads corba.ads corba-abstractbase.ads corba-current.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-current.ads portableserver-current.adb sequences.ads sequences-unbounded.ads
portableserver-poa.lo: broca.ads broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-forward.adb corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poa.adb portableserver-poamanager.ads portableserver-servantactivator.ads portableserver-servantactivator-impl.ads portableserver-servantlocator.ads portableserver-servantlocator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads
portableserver-poamanager.lo: broca.ads broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-poamanager.ads portableserver-poamanager.adb portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads
portableserver-servantactivator-impl.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator.ads portableserver-servantactivator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads
portableserver-servantactivator.lo: broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator.ads portableserver-servantactivator.adb portableserver-servantactivator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads
portableserver-servantlocator-impl.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantlocator.ads portableserver-servantlocator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads
portableserver-servantlocator.lo: broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantlocator.ads portableserver-servantlocator.adb portableserver-servantlocator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads
portableserver-servantmanager-impl.lo: broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads broca-sequences.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads portableserver-servantmanager-impl.adb sequences.ads sequences-unbounded.ads
portableserver-servantmanager.lo: broca.ads broca-buffers.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-impl.ads corba-object.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads
portableserver.lo: broca.ads broca-buffers.ads broca-configuration.ads broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-orb.ads broca-poa.ads broca-refs.ads broca-sequences.ads broca-soft_links.ads corba.ads corba-abstractbase.ads corba-forward.ads corba-forward.adb corba-impl.ads corba-object.ads corba-orb.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver.adb portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads sequences-unbounded.adb
sequences-bounded.lo: sequences.ads sequences-bounded.ads sequences-bounded.adb
sequences-unbounded.lo: sequences.ads sequences-unbounded.ads sequences-unbounded.adb
sequences.lo: sequences.ads
