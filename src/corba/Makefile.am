SUBDIRS = .

alidir = $(libdir)/polyorb-corba

ADA_SPECS_WITH_BODY = \
	corba-abstractbase.ads \
	corba-context.ads \
	corba-contextlist.ads \
	corba-exceptionlist.ads \
	corba-fixed_point.ads \
	corba-forward.ads \
	corba-impl.ads \
	corba-nvlist.ads \
	corba-object-helper.ads \
	corba-object.ads \
	corba-orb.ads \
	corba-request.ads \
	corba-serverrequest.ads \
	corba.ads \
	polyorb-binding_data-iiop.ads \
	polyorb-corba_p-exceptions-stack.ads \
	polyorb-corba_p-exceptions.ads \
	polyorb-corba_p-names.ads \
	polyorb-corba_p-server_tools.ads \
	polyorb-protocols-giop-giop_1_0.ads \
	polyorb-protocols-giop-giop_1_1.ads \
	polyorb-protocols-giop-giop_1_2.ads \
	polyorb-protocols-giop.ads \
	polyorb-references-ior.ads \
	polyorb-representations-cdr.ads \
	portableserver-adapteractivator.ads \
	portableserver-poa.ads \
	portableserver-poamanager.ads \
	portableserver.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	corba-policy_types.ads \
	corba-policy_values.ads \
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
	$(srcdir)/../../support/mkdep $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
corba-abstractbase.lo:corba-abstractbase.adb corba-abstractbase.ads corba-impl.ads corba.ads 
corba-context.lo:corba-abstractbase.ads corba-context.adb corba-context.ads corba-impl.ads corba-nvlist.ads corba.ads 
corba-contextlist.lo:corba-abstractbase.ads corba-contextlist.adb corba-contextlist.ads corba-impl.ads corba.ads 
corba-exceptionlist.lo:corba-abstractbase.ads corba-exceptionlist.adb corba-exceptionlist.ads corba-impl.ads corba.ads 
corba-fixed_point.lo:corba-fixed_point.adb corba-fixed_point.ads corba.ads polyorb-corba_p-exceptions-stack.ads polyorb-corba_p-exceptions.ads polyorb-corba_p.ads polyorb-representations-cdr.adb polyorb-representations-cdr.ads 
corba-forward.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba.ads 
corba-impl.lo:corba-impl.adb corba-impl.ads corba.ads 
corba-nvlist.lo:corba-abstractbase.ads corba-impl.ads corba-nvlist.adb corba-nvlist.ads corba.ads 
corba-object-helper.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object-helper.adb corba-object-helper.ads corba-object.ads corba-request.ads corba.ads 
corba-object.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.adb corba-object.ads corba-orb.ads corba-request.ads corba.ads 
corba-orb.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.adb corba-orb.ads corba-request.ads corba.ads polyorb-references-ior.ads 
corba-policy_types.lo:corba-policy_types.ads corba.ads 
corba-policy_values.lo:corba-policy_values.ads corba.ads 
corba-request.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.adb corba-request.ads corba.ads 
corba-serverrequest.lo:corba-abstractbase.ads corba-impl.ads corba-nvlist.ads corba-serverrequest.adb corba-serverrequest.ads corba.ads 
corba.lo:corba.adb corba.ads polyorb-corba_p-exceptions.ads polyorb-corba_p.ads 
polyorb-binding_data-iiop.lo:corba.ads polyorb-binding_data-iiop.adb polyorb-binding_data-iiop.ads polyorb-protocols-giop.ads polyorb-references-ior.ads polyorb-representations-cdr.ads 
polyorb-corba_p-exceptions-stack.lo:corba.ads polyorb-corba_p-exceptions-stack.adb polyorb-corba_p-exceptions-stack.ads polyorb-corba_p-exceptions.ads polyorb-corba_p.ads 
polyorb-corba_p-exceptions.lo:corba.ads polyorb-corba_p-exceptions-stack.ads polyorb-corba_p-exceptions.adb polyorb-corba_p-exceptions.ads polyorb-corba_p-names.ads polyorb-corba_p.ads 
polyorb-corba_p-names.lo:polyorb-corba_p-names.adb polyorb-corba_p-names.ads polyorb-corba_p.ads 
polyorb-corba_p-server_tools.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-orb.ads corba-request.ads corba-serverrequest.ads corba.ads polyorb-corba_p-server_tools.adb polyorb-corba_p-server_tools.ads polyorb-corba_p.ads portableserver-adapteractivator.ads portableserver-poa.ads portableserver-poamanager.ads portableserver-servantmanager.ads portableserver.ads 
polyorb-corba_p.lo:polyorb-corba_p.ads 
polyorb-protocols-giop-giop_1_0.lo:corba.ads polyorb-binding_data-iiop.ads polyorb-protocols-giop-giop_1_0.adb polyorb-protocols-giop-giop_1_0.ads polyorb-protocols-giop.ads polyorb-references-ior.ads polyorb-representations-cdr.ads 
polyorb-protocols-giop-giop_1_1.lo:corba.ads polyorb-binding_data-iiop.ads polyorb-protocols-giop-giop_1_1.adb polyorb-protocols-giop-giop_1_1.ads polyorb-protocols-giop.ads polyorb-references-ior.ads polyorb-representations-cdr.ads 
polyorb-protocols-giop-giop_1_2.lo:corba.ads polyorb-binding_data-iiop.ads polyorb-protocols-giop-giop_1_2.adb polyorb-protocols-giop-giop_1_2.ads polyorb-protocols-giop.ads polyorb-references-ior.ads polyorb-representations-cdr.ads 
polyorb-protocols-giop.lo:corba.ads polyorb-binding_data-iiop.ads polyorb-protocols-giop-giop_1_0.ads polyorb-protocols-giop-giop_1_1.ads polyorb-protocols-giop-giop_1_2.ads polyorb-protocols-giop.adb polyorb-protocols-giop.ads polyorb-references-ior.ads polyorb-representations-cdr.ads 
polyorb-references-ior.lo:corba.ads polyorb-corba_p-exceptions.ads polyorb-corba_p.ads polyorb-references-ior.adb polyorb-references-ior.ads polyorb-representations-cdr.ads 
polyorb-representations-cdr.lo:corba.ads polyorb-corba_p-exceptions-stack.ads polyorb-corba_p-exceptions.ads polyorb-corba_p.ads polyorb-representations-cdr.adb polyorb-representations-cdr.ads 
portableserver-adapteractivator.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-serverrequest.ads corba.ads portableserver-adapteractivator.adb portableserver-adapteractivator.ads portableserver.ads 
portableserver-poa.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-serverrequest.ads corba.ads polyorb-corba_p-exceptions.ads polyorb-corba_p.ads portableserver-adapteractivator.ads portableserver-poa.adb portableserver-poa.ads portableserver-poamanager.ads portableserver-servantmanager.ads portableserver.ads 
portableserver-poamanager.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-serverrequest.ads corba.ads polyorb-corba_p-exceptions.ads polyorb-corba_p.ads portableserver-poamanager.adb portableserver-poamanager.ads portableserver.ads 
portableserver-servantmanager.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-serverrequest.ads corba.ads portableserver-servantmanager.ads portableserver.ads 
portableserver.lo:corba-abstractbase.ads corba-context.ads corba-contextlist.ads corba-exceptionlist.ads corba-forward.adb corba-forward.ads corba-impl.ads corba-nvlist.ads corba-object.ads corba-request.ads corba-serverrequest.ads corba.ads portableserver.adb portableserver.ads 
