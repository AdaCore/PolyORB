AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support

corba_dir =
dsa_dir   =
moma_dir  =

giop_dir  =
soap_dir  =
srp_dir   =

if DEBUG
SUBDIRS = compilers idls src cos examples testsuite @APPLI_DIRS@ @PROTO_DIRS@
else
SUBDIRS = compilers idls src cos examples @APPLI_DIRS@ @PROTO_DIRS@
endif

CLEANFILES = config-stamp
DISTCLEANFILES = support/adacompiler support/native-adacompiler support/linker polyorb-config

depend:
	cd src && ${MAKE} depend

run_tests:
	cd testsuite && ${MAKE} run_tests

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) polyorb-config $(DESTDIR)$(exec_prefix)/bin/polyorb-config
