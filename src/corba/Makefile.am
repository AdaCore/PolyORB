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

depend: all
	$(srcdir)/../support/mkdep $(srcdir)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
