AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support

corba_dir      = idls cos
dsa_dir        =
moma_dir       =
web_common_dir =

giop_dir       =
soap_dir       =
srp_dir        =

SUBDIRS = support compilers @APPLI_DIRS@ @PROTO_DIRS@ @LIBS_DIRS@ src tools

CLEANFILES = config-stamp
DISTCLEANFILES = support/adacompiler support/native-adacompiler support/linker polyorb-config

depend:
	cd src && ${MAKE} depend

run_tests:
	cd testsuite && ${MAKE} run_tests

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) polyorb-config $(DESTDIR)$(bindir)/polyorb-config
