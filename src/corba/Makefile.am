SUBDIRS = . rtcorba

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/corba

ADA_SPECS_WITH_BODY = \
	corba-abstractbase.ads \
	corba-context.ads \
	corba-contextlist.ads \
	corba-exceptionlist.ads \
	corba-fixed_point.ads \
	corba-forward.ads \
	corba-helper.ads \
	corba-impl.ads \
	corba-nvlist.ads \
	corba-object-helper.ads \
	corba-object.ads \
	corba-orb.ads \
	corba-policy.ads \
	corba-request.ads \
	corba-serverrequest.ads \
	corba.ads \
	polyorb-corba_p-adapteractivator.ads \
	polyorb-corba_p-exceptions.ads \
	polyorb-corba_p-initial_references.ads \
	polyorb-corba_p-names.ads \
	polyorb-corba_p-poa_config.ads \
	polyorb-corba_p-policy.ads \
	polyorb-corba_p-servantactivator.ads \
	polyorb-corba_p-servantlocator.ads \
	polyorb-corba_p-server_tools.ads \
	portableserver-adapteractivator.ads \
	portableserver-current.ads \
	portableserver-idassignmentpolicy.ads \
	portableserver-iduniquenesspolicy.ads \
	portableserver-implicitactivationpolicy.ads \
	portableserver-lifespanpolicy.ads \
	portableserver-poa.ads \
	portableserver-poa-goa.ads \
	portableserver-poamanager.ads \
	portableserver-requestprocessingpolicy.ads \
	portableserver-servantactivator.ads \
	portableserver-servantlocator.ads \
	portableserver-servantretentionpolicy.ads \
	portableserver-threadpolicy.ads \
	portableserver.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	corba-current.ads \
	corba-local.ads \
	corba-sequences-bounded.ads \
	corba-sequences-unbounded.ads \
	corba-sequences.ads \
	polyorb-corba_p.ads \
	portableserver-servantmanager.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-corba.la
libpolyorb_corba_la_SOURCES =
libpolyorb_corba_la_LIBADD = $(ADA_LIBADD)
libpolyorb_corba_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

LINK = $(LIBTOOL) $(ADA) -o libpolyorb-corba.la

ADAFLAGS = -I. -I$(srcdir) -I.. -I$(srcdir)/.. $(GNATFLAGS)

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
	for f in $(srcdir)/*.ad[bs] *.ali; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done

depend-local:
	for d in $(SUBDIRS);do \
		(cd $$d && ${MAKE} depend-local-local); \
	done

depend-local-local: all
	$(srcdir)/../../support/mkdep $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
corba-abstractbase.lo:corba-abstractbase.adb corba-abstractbase.ads corba.ads corba-impl.ads 
corba.lo:corba.adb corba.ads polyorb-corba_p.ads polyorb-corba_p-exceptions.ads 
corba-context.lo:corba-abstractbase.ads corba.ads corba-context.adb corba-context.ads corba-impl.ads corba-nvlist.ads 
corba-contextlist.lo:corba-abstractbase.ads corba.ads corba-contextlist.adb corba-contextlist.ads corba-impl.ads corba-sequences.ads corba-sequences-unbounded.ads 
corba-current.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-current.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads 
corba-exceptionlist.lo:corba-abstractbase.ads corba.ads corba-exceptionlist.adb corba-exceptionlist.ads corba-impl.ads 
corba-fixed_point.lo:corba.ads corba-fixed_point.adb corba-fixed_point.ads 
corba-forward.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads 
corba-helper.lo:corba.ads corba-helper.adb corba-helper.ads 
corba-impl.lo:corba.ads corba-impl.adb corba-impl.ads 
corba-local.lo:corba.ads corba-local.ads 
corba-nvlist.lo:corba-abstractbase.ads corba.ads corba-impl.ads corba-nvlist.adb corba-nvlist.ads 
corba-object.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.adb corba-object.ads corba-object-helper.ads corba-orb.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads polyorb-corba_p.ads polyorb-corba_p-names.ads 
corba-object-helper.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-object-helper.adb corba-object-helper.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads 
corba-orb.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.adb corba-orb.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads polyorb-corba_p.ads polyorb-corba_p-initial_references.ads polyorb-corba_p-policy.ads 
corba-policy.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-policy.adb corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads polyorb-corba_p.ads polyorb-corba_p-policy.ads 
corba-request.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.adb corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads polyorb-corba_p.ads polyorb-corba_p-exceptions.ads 
corba-sequences.lo:corba.ads corba-sequences.ads 
corba-sequences-bounded.lo:corba.ads corba-sequences.ads corba-sequences-bounded.ads 
corba-sequences-unbounded.lo:corba.ads corba-sequences.ads corba-sequences-unbounded.ads 
corba-serverrequest.lo:corba-abstractbase.ads corba.ads corba-impl.ads corba-nvlist.ads corba-serverrequest.adb corba-serverrequest.ads 
polyorb-corba_p-adapteractivator.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p-adapteractivator.adb polyorb-corba_p-adapteractivator.ads polyorb-corba_p.ads portableserver-adapteractivator.ads portableserver.ads 
polyorb-corba_p.lo:polyorb-corba_p.ads 
polyorb-corba_p-exceptions.lo:corba.ads polyorb-corba_p.ads polyorb-corba_p-exceptions.adb polyorb-corba_p-exceptions.ads 
polyorb-corba_p-initial_references.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads polyorb-corba_p.ads polyorb-corba_p-initial_references.adb polyorb-corba_p-initial_references.ads 
polyorb-corba_p-names.lo:polyorb-corba_p.ads polyorb-corba_p-names.adb polyorb-corba_p-names.ads 
polyorb-corba_p-poa_config.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-poa_config.adb polyorb-corba_p-poa_config.ads portableserver.ads portableserver-idassignmentpolicy.ads portableserver-iduniquenesspolicy.ads portableserver-implicitactivationpolicy.ads portableserver-lifespanpolicy.ads portableserver-requestprocessingpolicy.ads portableserver-servantretentionpolicy.ads portableserver-threadpolicy.ads 
polyorb-corba_p-policy.lo:corba.ads polyorb-corba_p.ads polyorb-corba_p-policy.adb polyorb-corba_p-policy.ads 
polyorb-corba_p-servantactivator.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-servantactivator.adb polyorb-corba_p-servantactivator.ads portableserver.ads portableserver-servantactivator.ads portableserver-servantmanager.ads 
polyorb-corba_p-servantlocator.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-servantlocator.adb polyorb-corba_p-servantlocator.ads portableserver.ads portableserver-servantlocator.ads portableserver-servantmanager.ads 
polyorb-corba_p-server_tools.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-exceptions.ads polyorb-corba_p-server_tools.adb polyorb-corba_p-server_tools.ads portableserver-adapteractivator.ads portableserver.ads portableserver-idassignmentpolicy.ads portableserver-iduniquenesspolicy.ads portableserver-implicitactivationpolicy.ads portableserver-lifespanpolicy.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-requestprocessingpolicy.ads portableserver-servantmanager.ads portableserver-servantretentionpolicy.ads portableserver-threadpolicy.ads 
portableserver-adapteractivator.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads portableserver-adapteractivator.adb portableserver-adapteractivator.ads portableserver.ads 
portableserver.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-names.ads portableserver.adb portableserver.ads 
portableserver-current.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-current.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-initial_references.ads portableserver.ads portableserver-current.adb portableserver-current.ads 
portableserver-idassignmentpolicy.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-policy.ads portableserver.ads portableserver-idassignmentpolicy.adb portableserver-idassignmentpolicy.ads 
portableserver-iduniquenesspolicy.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-policy.ads portableserver.ads portableserver-iduniquenesspolicy.adb portableserver-iduniquenesspolicy.ads 
portableserver-implicitactivationpolicy.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-policy.ads portableserver.ads portableserver-implicitactivationpolicy.adb portableserver-implicitactivationpolicy.ads 
portableserver-lifespanpolicy.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-policy.ads portableserver.ads portableserver-lifespanpolicy.adb portableserver-lifespanpolicy.ads 
portableserver-poa.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p-adapteractivator.ads polyorb-corba_p.ads polyorb-corba_p-exceptions.ads polyorb-corba_p-poa_config.ads polyorb-corba_p-servantactivator.ads polyorb-corba_p-servantlocator.ads portableserver-adapteractivator.ads portableserver.ads portableserver-idassignmentpolicy.ads portableserver-iduniquenesspolicy.ads portableserver-implicitactivationpolicy.ads portableserver-lifespanpolicy.ads portableserver-poa.adb portableserver-poa.ads portableserver-poamanager.ads portableserver-requestprocessingpolicy.ads portableserver-servantactivator.ads portableserver-servantlocator.ads portableserver-servantmanager.ads portableserver-servantretentionpolicy.ads portableserver-threadpolicy.ads 
portableserver-poa-goa.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-exceptions.ads portableserver-adapteractivator.ads portableserver.ads portableserver-idassignmentpolicy.ads portableserver-iduniquenesspolicy.ads portableserver-implicitactivationpolicy.ads portableserver-lifespanpolicy.ads portableserver-poa.ads portableserver-poa-goa.adb portableserver-poa-goa.ads portableserver-poamanager.ads portableserver-requestprocessingpolicy.ads portableserver-servantmanager.ads portableserver-servantretentionpolicy.ads portableserver-threadpolicy.ads 
portableserver-poamanager.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-exceptions.ads portableserver.ads portableserver-poamanager.adb portableserver-poamanager.ads 
portableserver-requestprocessingpolicy.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-policy.ads portableserver.ads portableserver-requestprocessingpolicy.adb portableserver-requestprocessingpolicy.ads 
portableserver-servantactivator.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads portableserver.ads portableserver-servantactivator.adb portableserver-servantactivator.ads portableserver-servantmanager.ads 
portableserver-servantlocator.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads portableserver.ads portableserver-servantlocator.adb portableserver-servantlocator.ads portableserver-servantmanager.ads 
portableserver-servantmanager.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads portableserver.ads portableserver-servantmanager.ads 
portableserver-servantretentionpolicy.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-policy.ads portableserver.ads portableserver-servantretentionpolicy.adb portableserver-servantretentionpolicy.ads 
portableserver-threadpolicy.lo:corba-abstractbase.ads corba.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-policy.ads corba-request.ads corba-sequences.ads corba-sequences-unbounded.ads corba-serverrequest.ads polyorb-corba_p.ads polyorb-corba_p-policy.ads portableserver.ads portableserver-threadpolicy.adb portableserver-threadpolicy.ads 
