alidir = $(libdir)/adabroker

ADA_SPECS_WITH_BODY = \
	   broca-debug.ads \
	   broca-opaque-chunk_pools.ads \
	   broca-buffers.ads \
           broca-environment.ads \
	   broca-protected_objects.ads \
	   broca-server_tools.ads \
	   broca-configuration.ads \
	   broca-soft_links.ads \
	   broca-buffers-io_operations.ads \
	   broca-exceptions.ads \
	   broca-exceptions-stack.ads \
	   broca-flags.ads \
	   broca-giop.ads \
	   broca-iiop.ads \
	   broca-inet_server.ads \
	   broca-iop.ads \
	   broca-ior.ads \
	   broca-locks.ads \
           broca-names.ads \
	   broca-cdr.ads \
	   broca-object.ads \
	   broca-orb.ads \
	   broca-operation_store.ads \
	   broca-poa.ads \
	   broca-policy.ads \
	   broca-refs.ads \
	   broca-repository.ads \
	   broca-rootpoa.ads \
	   broca-sequences.ads \
	   broca-server.ads \
           broca-sockets.ads \
	   broca-stream.ads \
	   broca-task_attributes.ads \
	   broca-vararray.ads \
	   broca-value-operation_store.ads \
	   broca-value-stream.ads \
	   broca-value-value_skel.ads \
	   corba-forward.ads \
	   corba-object.ads \
	   corba-object-helper.ads \
	   corba-orb.ads \
	   corba-nvlist.ads \
	   corba-exceptionlist.ads \
	   corba-contextlist.ads \
	   corba-request.ads \
	   corba-context.ads \
	   corba.ads \
	   corba-abstractbase.ads \
	   corba-impl.ads \
	   corba-value-forward.ads \
	   corba-value.ads \
	   corba-value-box.ads \
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
	   sequences-unbounded.ads \
	   sequences-unbounded-search.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	   broca.ads \
	   broca-opaque.ads \
           broca-parameters.ads \
	   broca-value.ads \
	   corba-current.ads \
	   corba-sequences.ads \
	   corba-sequences-bounded.ads \
	   corba-sequences-unbounded.ads \
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
broca-buffers-io_operations.lo:broca.ads broca-buffers.ads broca-buffers-io_operations.adb broca-buffers-io_operations.ads broca-opaque.ads broca-opaque-chunk_pools.ads 
broca-buffers.lo:broca.ads broca-buffers.adb broca-buffers.ads broca-debug.adb broca-debug.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.adb broca-opaque-chunk_pools.ads corba.ads 
broca-cdr.lo:broca.ads broca-buffers.ads broca-cdr.adb broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-exceptions-stack.ads broca-iop.ads broca-locks.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads broca-value.ads broca-value-operation_store.ads broca-value-stream.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-object-helper.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-value.ads sequences.ads sequences-unbounded.ads sequences-unbounded-search.ads 
broca-configuration.lo:broca.ads broca-configuration.adb broca-configuration.ads broca-protected_objects.ads broca-soft_links.ads 
broca-debug.lo:broca.ads broca-debug.adb broca-debug.ads 
broca-environment.lo:broca.ads broca-environment.adb broca-environment.ads 
broca-exceptions-stack.lo:broca.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-exceptions-stack.adb broca-exceptions-stack.ads broca-locks.ads broca-soft_links.ads corba.ads 
broca-exceptions.lo:broca.ads broca-exceptions.adb broca-exceptions.ads broca-exceptions-stack.ads broca-locks.ads broca-names.ads corba.ads 
broca-flags.lo:broca.ads broca-flags.adb broca-flags.ads broca-locks.ads corba.ads 
broca-giop.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-giop.adb broca-giop.ads broca-iop.ads broca-locks.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads 
broca-iiop.lo:broca.ads broca-buffers.ads broca-buffers-io_operations.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-iiop.adb broca-iiop.ads broca-iop.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads broca-sockets.ads broca-soft_links.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.adb sequences-unbounded.ads 
broca-inet_server.lo:broca.ads broca-buffers.ads broca-debug.adb broca-debug.ads broca-environment.ads broca-exceptions.ads broca-giop.ads broca-iiop.ads broca-inet_server.adb broca-inet_server.ads broca-iop.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads broca-sequences.ads broca-server.ads broca-sockets.ads broca-stream.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads 
broca-iop.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-iop.adb broca-iop.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads 
broca-ior.lo:broca.ads broca-buffers.ads broca-exceptions.ads broca-ior.adb broca-ior.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads corba.ads 
broca-locks.lo:broca.ads broca-debug.adb broca-debug.ads broca-locks.adb broca-locks.ads 
broca-names.lo:broca.ads broca-names.adb broca-names.ads 
broca-object.lo:broca.ads broca-buffers.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-iop.ads broca-locks.ads broca-object.adb broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads corba.ads corba-impl.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads 
broca-opaque-chunk_pools.lo:broca.ads broca-opaque.ads broca-opaque-chunk_pools.adb broca-opaque-chunk_pools.ads 
broca-opaque.lo:broca.ads broca-opaque.ads 
broca-operation_store.lo:broca.ads broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-operation_store.adb broca-operation_store.ads broca-refs.ads broca-repository.ads broca-soft_links.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads sequences.ads sequences-unbounded.ads 
broca-orb.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-environment.ads broca-exceptions.ads broca-iiop.ads broca-iop.ads broca-locks.ads broca-names.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-orb.adb broca-orb.ads broca-poa.ads broca-refs.ads broca-repository.ads broca-sequences.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantlocator.ads portableserver-servantlocator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.adb sequences-unbounded.ads 
broca-parameters.lo:broca.ads broca-parameters.ads 
broca-poa.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-iop.ads broca-locks.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.adb broca-poa.ads broca-refs.ads broca-sequences.ads broca-server.ads broca-stream.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads 
broca-policy.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-policy.adb broca-policy.ads broca-refs.ads corba.ads 
broca-protected_objects.lo:broca.ads broca-protected_objects.adb broca-protected_objects.ads broca-soft_links.ads 
broca-refs.lo:broca.ads broca-buffers.ads broca-debug.adb broca-debug.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.adb broca-refs.ads 
broca-repository.lo:broca.ads broca-buffers.ads broca-debug.adb broca-debug.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-repository.adb broca-repository.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads sequences.ads sequences-unbounded.ads 
broca-rootpoa.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-flags.ads broca-giop.ads broca-iop.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-orb.ads broca-poa.ads broca-refs.ads broca-rootpoa.adb broca-rootpoa.ads broca-sequences.ads broca-server.ads broca-stream.ads broca-task_attributes.ads broca-vararray.adb broca-vararray.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator.ads portableserver-servantactivator-impl.ads portableserver-servantlocator.ads portableserver-servantlocator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads 
broca-sequences.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.adb broca-sequences.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.adb sequences-unbounded.ads 
broca-server.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-flags.ads broca-giop.ads broca-iop.ads broca-locks.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-orb.ads broca-parameters.ads broca-poa.ads broca-refs.ads broca-sequences.ads broca-server.adb broca-server.ads broca-stream.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads 
broca-server_tools.lo:broca.ads broca-buffers.ads broca-debug.adb broca-debug.ads broca-inet_server.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-parameters.ads broca-refs.ads broca-rootpoa.ads broca-server_tools.adb broca-server_tools.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads 
broca-sockets.lo:broca.ads broca-debug.adb broca-debug.ads broca-opaque.ads broca-sockets.adb broca-sockets.ads 
broca-soft_links.lo:broca.ads broca-soft_links.adb broca-soft_links.ads 
broca-stream.lo:broca.ads broca-buffers.ads broca-buffers-io_operations.ads broca-debug.adb broca-debug.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-sockets.ads broca-stream.adb broca-stream.ads 
broca-task_attributes.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-task_attributes.adb broca-task_attributes.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads sequences.ads sequences-unbounded.ads 
broca-value-cdr.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-value.ads broca-value-cdr.adb broca-value-cdr.ads broca-value-operation_store.ads broca-value-stream.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-value.ads sequences.ads sequences-unbounded.ads sequences-unbounded-search.ads 
broca-value-operation_store.lo:broca.ads broca-exceptions.ads broca-locks.ads broca-soft_links.ads broca-value.ads broca-value-operation_store.adb broca-value-operation_store.ads corba.ads 
broca-value-stream.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-soft_links.ads broca-value.ads broca-value-operation_store.adb broca-value-operation_store.ads broca-value-stream.adb broca-value-stream.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-value.ads sequences.ads sequences-unbounded.adb sequences-unbounded.ads sequences-unbounded-search.adb sequences-unbounded-search.ads 
broca-value-value_skel.lo:broca.ads broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-names.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-repository.ads broca-soft_links.ads broca-value.ads broca-value-operation_store.adb broca-value-operation_store.ads broca-value-value_skel.adb broca-value-value_skel.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-value.ads sequences.ads sequences-unbounded.ads 
broca-value.lo:broca.ads broca-value.ads 
broca-vararray.lo:broca.ads broca-vararray.adb broca-vararray.ads 
broca.lo:broca.ads 
corba-abstractbase.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.adb corba-abstractbase.ads corba.ads corba-impl.ads 
corba-context.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.adb corba-context.ads corba-impl.ads corba-nvlist.ads sequences.ads sequences-unbounded.ads 
corba-contextlist.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-contextlist.adb corba-contextlist.ads corba-impl.ads sequences.ads sequences-unbounded.adb sequences-unbounded.ads 
corba-current.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-current.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads sequences.ads sequences-unbounded.ads 
corba-exceptionlist.lo:broca.ads broca-buffers.ads broca-debug.adb broca-debug.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-exceptionlist.adb corba-exceptionlist.ads corba-impl.ads sequences.ads sequences-unbounded.adb sequences-unbounded.ads sequences-unbounded-search.adb sequences-unbounded-search.ads 
corba-forward.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads sequences.ads sequences-unbounded.ads 
corba-impl.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba.ads corba-impl.adb corba-impl.ads 
corba-nvlist.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.adb corba-nvlist.ads corba-object.ads corba-request.ads sequences.ads sequences-unbounded.adb sequences-unbounded.ads 
corba-object-helper.lo:broca.ads broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-object-helper.adb corba-object-helper.ads corba-request.ads sequences.ads sequences-unbounded.ads 
corba-object.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-exceptions.ads broca-giop.ads broca-iop.ads broca-ior.ads broca-locks.ads broca-names.ads broca-object.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-repository.ads broca-sequences.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.adb corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads 
corba-orb.lo:broca.ads broca-buffers.ads broca-debug.adb broca-debug.ads broca-ior.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-orb.ads broca-poa.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.adb corba-orb.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.adb sequences-unbounded.ads 
corba-request.lo:broca.ads broca-buffers.ads broca-cdr.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-giop.ads broca-iop.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-sequences.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.adb corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.ads 
corba-sequences-bounded.lo:broca.ads broca-locks.ads corba.ads corba-sequences.ads corba-sequences-bounded.ads sequences.ads sequences-bounded.adb sequences-bounded.ads 
corba-sequences-unbounded.lo:broca.ads broca-locks.ads corba.ads corba-sequences.ads corba-sequences-unbounded.ads sequences.ads sequences-unbounded.adb sequences-unbounded.ads 
corba-sequences.lo:broca.ads broca-locks.ads corba.ads corba-sequences.ads sequences.ads 
corba-value-box.lo:broca.ads broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-impl.ads corba-value.ads corba-value-box.adb corba-value-box.ads 
corba-value-forward.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-impl.ads corba-value.ads corba-value-forward.adb corba-value-forward.ads 
corba-value.lo:broca.ads broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-value.ads broca-value-operation_store.ads broca-value-value_skel.ads corba-abstractbase.ads corba.ads corba-impl.ads corba-value.adb corba-value.ads 
corba.lo:broca.ads broca-configuration.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-locks.ads corba.adb corba.ads 
portableserver-adapteractivator-impl.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver-adapteractivator-impl.adb portableserver-adapteractivator-impl.ads portableserver.ads sequences.ads sequences-unbounded.ads 
portableserver-adapteractivator.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.adb portableserver-adapteractivator.ads portableserver-adapteractivator-impl.ads portableserver.ads sequences.ads sequences-unbounded.ads 
portableserver-current.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads broca-task_attributes.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-current.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-current.adb portableserver-current.ads sequences.ads sequences-unbounded.ads 
portableserver-poa.lo:broca.ads broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-poa.adb portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator.ads portableserver-servantactivator-impl.ads portableserver-servantlocator.ads portableserver-servantlocator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads 
portableserver-poamanager.lo:broca.ads broca-buffers.ads broca-exceptions.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-poa.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-poamanager.adb portableserver-poamanager.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads 
portableserver-servantactivator-impl.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator.ads portableserver-servantactivator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads 
portableserver-servantactivator.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantactivator.adb portableserver-servantactivator.ads portableserver-servantactivator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads 
portableserver-servantlocator-impl.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantlocator.ads portableserver-servantlocator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads 
portableserver-servantlocator.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantlocator.adb portableserver-servantlocator.ads portableserver-servantlocator-impl.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads 
portableserver-servantmanager-impl.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-servantmanager.ads portableserver-servantmanager-impl.adb portableserver-servantmanager-impl.ads sequences.ads sequences-unbounded.ads 
portableserver-servantmanager.lo:broca.ads broca-buffers.ads broca-locks.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-refs.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.ads 
portableserver.lo:broca.ads broca-buffers.ads broca-configuration.ads broca-debug.adb broca-debug.ads broca-exceptions.ads broca-locks.ads broca-names.ads broca-opaque.ads broca-opaque-chunk_pools.ads broca-orb.ads broca-poa.ads broca-refs.ads broca-sequences.ads broca-soft_links.ads corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads portableserver-adapteractivator.ads portableserver.adb portableserver.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantmanager.ads sequences.ads sequences-unbounded.adb sequences-unbounded.ads 
sequences-bounded.lo:sequences.ads sequences-bounded.adb sequences-bounded.ads 
sequences-unbounded-search.lo:sequences.ads sequences-unbounded.ads sequences-unbounded-search.adb sequences-unbounded-search.ads 
sequences-unbounded.lo:sequences.ads sequences-unbounded.adb sequences-unbounded.ads 
sequences.lo:sequences.ads 
