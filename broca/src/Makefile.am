alidir = $(libdir)/adabroker

ADA_SPECS_WITH_BODY = \
	   broca-debug.ads \
	   broca-opaque-chunk_pools.ads \
	   broca-buffers.ads \
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
	   broca-ior.ads \
	   broca-locks.ads \
           broca-names.ads \
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
	   sequences-unbounded.ads \
	   sequences-unbounded-search.ads

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
