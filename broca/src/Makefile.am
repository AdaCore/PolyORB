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
	   corba-object-helper.ads \
	   corba-orb.ads \
	   corba-nvlist.ads \
	   corba-request.ads \
	   corba-context.ads \
	   corba.ads \
	   corba-abstractbase.ads \
	   corba-impl.ads \
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

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

bin_SCRIPTS = adabroker-config
lib_LTLIBRARIES = libbroca.la
libbroca_la_SOURCES =
libbroca_la_LIBADD = $(ADA_LIBADD)
libbroca_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libbroca.la

ADAFLAGS = $(GNATFLAGS) \
	-I. -I$(srcdir) $(ADASOCKETS_INCLUDE)

ADACOMPILER = ../support/adacompiler

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

depend: all
	$(srcdir)/../support/mkdep $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
broca-basic_startup.lo:broca-basic_startup.adb broca-basic_startup.ads broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-rootpoa.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
broca-buffers-io_operations.lo:broca-buffers-io_operations.adb broca-buffers-io_operations.ads broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca.ads 
broca-buffers.lo:broca-buffers.adb broca-buffers.ads broca-debug.adb broca-debug.ads broca-opaque-chunk_pools.adb broca-opaque-chunk_pools.ads broca-opaque.ads broca.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-cdr.lo:broca-buffers.ads broca-cdr.adb broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-iop.ads broca-object.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-sequences.ads broca.ads corba-abstractbase.ads corba-context.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-configuration.lo:broca-configuration.adb broca-configuration.ads broca-protected_objects.ads broca-soft_links.ads broca.ads 
broca-debug.lo:broca-debug.adb broca-debug.ads broca.ads 
broca-exceptions-stack.lo:broca-buffers.ads broca-debug.adb broca-debug.ads broca-exceptions-stack.adb broca-exceptions-stack.ads broca-exceptions.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-soft_links.ads broca.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-exceptions.lo:broca-buffers.ads broca-exceptions.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-flags.lo:broca-flags.adb broca-flags.ads broca.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-giop.lo:broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-giop.adb broca-giop.ads broca-iop.ads broca-object.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-sequences.ads broca.ads corba-abstractbase.ads corba-context.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-iiop.lo:broca-buffers-io_operations.ads broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-iiop.adb broca-iiop.ads broca-iop.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-sequences.ads broca.ads corba-abstractbase.ads corba-context.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-inet_server.lo:broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-flags.ads broca-giop.ads broca-iiop.ads broca-inet_server.adb broca-inet_server.ads broca-iop.ads broca-locks.ads broca-object.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-poa.ads broca-refs.ads broca-sequences.ads broca-server.ads broca-stream.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
broca-iop.lo:broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-iop.adb broca-iop.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-sequences.ads broca.ads corba-abstractbase.ads corba-context.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-ior.lo:broca-buffers.ads broca-exceptions.ads broca-ior.adb broca-ior.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-locks.lo:broca-locks.adb broca-locks.ads broca.ads 
broca-object.lo:broca-buffers.ads broca-iop.ads broca-object.adb broca-object.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-sequences.ads broca.ads corba-impl.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-opaque-chunk_pools.lo:broca-opaque-chunk_pools.adb broca-opaque-chunk_pools.ads broca-opaque.ads broca.ads 
broca-opaque.lo:broca-opaque.ads broca.ads 
broca-orb.lo:broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-iiop.ads broca-iop.ads broca-locks.ads broca-object.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-orb.adb broca-orb.ads broca-poa.ads broca-refs.ads broca-repository.ads broca-sequences.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.adb sequences-unbounded.ads sequences.ads 
broca-poa.lo:broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-poa.adb broca-poa.ads broca-refs.ads broca-sequences.ads broca-server.ads broca-stream.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
broca-policy.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-policy.adb broca-policy.ads broca-refs.ads broca.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-protected_objects.lo:broca-protected_objects.adb broca-protected_objects.ads broca-soft_links.ads broca.ads 
broca-refs.lo:broca-buffers.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.adb broca-refs.ads broca.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-repository.lo:broca-buffers.ads broca-debug.adb broca-debug.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-repository.adb broca-repository.ads broca.ads corba-abstractbase.ads corba-context.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba.ads sequences-unbounded.ads sequences.ads 
broca-rootpoa.lo:broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-flags.ads broca-inet_server.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-orb.ads broca-poa.ads broca-refs.ads broca-rootpoa.adb broca-rootpoa.ads broca-server.ads broca-stream.ads broca-vararray.adb broca-vararray.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator-impl.ads portableserver-servantactivator.ads portableserver-servantlocator-impl.ads portableserver-servantlocator.ads portableserver-servantmanager-impl.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
broca-sequences.lo:broca-buffers.ads broca-cdr.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-sequences.adb broca-sequences.ads broca.ads corba-abstractbase.ads corba-context.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads sequences-unbounded.adb sequences-unbounded.ads sequences.ads 
broca-server.lo:broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-flags.ads broca-giop.ads broca-iop.ads broca-locks.ads broca-object.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-orb.ads broca-poa.ads broca-refs.ads broca-sequences.ads broca-server.adb broca-server.ads broca-stream.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
broca-soft_links.lo:broca-soft_links.adb broca-soft_links.ads broca.ads 
broca-stream.lo:broca-buffers-io_operations.ads broca-buffers.ads broca-debug.adb broca-debug.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-stream.adb broca-stream.ads broca.ads 
broca-task_attributes.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-task_attributes.adb broca-task_attributes.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver.ads sequences-unbounded.ads sequences.ads 
broca-vararray.lo:broca-vararray.adb broca-vararray.ads broca.ads 
broca.lo:broca.ads 
corba-abstractbase.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.adb corba-abstractbase.ads corba-impl.ads corba.ads sequences-unbounded.ads sequences.ads 
corba-context.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.adb corba-context.ads corba-impl.ads corba-nvlist.ads corba.ads sequences-unbounded.ads sequences.ads 
corba-current.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-current.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba.ads sequences-unbounded.ads sequences.ads 
corba-forward.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba.ads sequences-unbounded.ads sequences.ads 
corba-impl.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-impl.adb corba-impl.ads corba.ads sequences-unbounded.ads sequences.ads 
corba-nvlist.lo:broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-impl.ads corba-nvlist.adb corba-nvlist.ads corba-object.ads corba-request.ads corba.ads sequences-unbounded.ads sequences.ads 
corba-object.lo:broca-buffers.ads broca-cdr.ads broca-ior.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-impl.ads corba-nvlist.ads corba-object.adb corba-object.ads corba-request.ads corba.ads sequences-unbounded.ads sequences.ads 
corba-orb.lo:broca-buffers.ads broca-debug.adb broca-debug.ads broca-ior.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-orb.ads broca-poa.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.adb corba-orb.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.adb sequences-unbounded.ads sequences.ads 
corba-request.lo:broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-giop.ads broca-iop.ads broca-object.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-sequences.ads broca.ads corba-abstractbase.ads corba-context.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.adb corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads sequences-unbounded.ads sequences.ads 
corba-sequences-bounded.lo:corba-sequences-bounded.ads corba-sequences.ads corba.ads sequences-bounded.adb sequences-bounded.ads sequences-unbounded.ads sequences.ads 
corba-sequences-unbounded.lo:corba-sequences-unbounded.ads corba-sequences.ads corba.ads sequences-unbounded.adb sequences-unbounded.ads sequences.ads 
corba-sequences.lo:corba-sequences.ads corba.ads sequences-unbounded.ads sequences.ads 
corba-value-forward.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-impl.ads corba-value-forward.adb corba-value-forward.ads corba-value.ads corba.ads sequences-unbounded.ads sequences.ads 
corba-value.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-impl.ads corba-value.ads corba.ads sequences-unbounded.ads sequences.ads 
corba.lo:broca-buffers.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca.ads corba.adb corba.ads sequences-unbounded.adb sequences-unbounded.ads sequences.ads 
portableserver-adapteractivator-impl.lo:broca-buffers.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-poa.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator-impl.adb portableserver-adapteractivator-impl.ads portableserver-adapteractivator.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver-adapteractivator.lo:broca-buffers.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-poa.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator-impl.ads portableserver-adapteractivator.adb portableserver-adapteractivator.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver-current.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca-task_attributes.ads broca.ads corba-abstractbase.ads corba-context.ads corba-current.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-current.adb portableserver-current.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver-poa.lo:broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-poa.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-poa.adb portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator-impl.ads portableserver-servantactivator.ads portableserver-servantlocator-impl.ads portableserver-servantlocator.ads portableserver-servantmanager-impl.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver-poamanager.lo:broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-poa.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-poamanager.adb portableserver-poamanager.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver-servantactivator-impl.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator-impl.ads portableserver-servantactivator.ads portableserver-servantmanager-impl.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver-servantactivator.lo:broca-buffers.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-poa.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator-impl.ads portableserver-servantactivator.adb portableserver-servantactivator.ads portableserver-servantmanager-impl.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver-servantlocator-impl.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantlocator-impl.ads portableserver-servantlocator.ads portableserver-servantmanager-impl.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver-servantlocator.lo:broca-buffers.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-poa.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantlocator-impl.ads portableserver-servantlocator.adb portableserver-servantlocator.ads portableserver-servantmanager-impl.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver-servantmanager-impl.lo:broca-buffers.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-poa.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-servantmanager-impl.adb portableserver-servantmanager-impl.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver-servantmanager.lo:broca-buffers.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-refs.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-servantmanager.ads portableserver.ads sequences-unbounded.ads sequences.ads 
portableserver.lo:broca-buffers.ads broca-configuration.ads broca-exceptions.ads broca-locks.ads broca-opaque-chunk_pools.ads broca-opaque.ads broca-orb.ads broca-poa.ads broca-refs.ads broca-sequences.ads broca-soft_links.ads broca.ads corba-abstractbase.ads corba-context.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-sequences-unbounded.ads corba-sequences.ads corba.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantmanager.ads portableserver.adb portableserver.ads sequences-unbounded.adb sequences-unbounded.ads sequences.ads 
sequences-bounded.lo:sequences-bounded.adb sequences-bounded.ads sequences.ads 
sequences-unbounded.lo:sequences-unbounded.adb sequences-unbounded.ads sequences.ads 
sequences.lo:sequences.ads 
