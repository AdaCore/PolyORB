AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support

corba_dir = idls cos
dsa_dir   =
moma_dir  =

giop_dir  =
soap_dir  =
srp_dir   =

SUBDIRS = compilers src examples @APPLI_DIRS@ @PROTO_DIRS@

CLEANFILES = config-stamp

depend:
	cd src && ${MAKE} depend

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) polyorb-config $(DESTDIR)$(exec_prefix)/bin/polyorb-config
