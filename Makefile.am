AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support

corba_dir      =
dsa_dir        =
moma_dir       =
devaws_dir     =
aws_common_dir =

giop_dir       =
soap_dir       =
srp_dir        =

if DEBUG
TESTSUITE=testsuite
else
TESTSUITE=
endif

SUBDIRS = support compilers idls src cos examples @DOC_DIR@ $(TESTSUITE) @APPLI_DIRS@ @PROTO_DIRS@ @LIBS_DIRS@

CLEANFILES = config-stamp
DISTCLEANFILES = support/adacompiler support/native-adacompiler support/linker polyorb-config

depend:
	cd src && ${MAKE} depend

run_tests:
	cd testsuite && ${MAKE} run_tests

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) polyorb-config $(DESTDIR)$(exec_prefix)/bin/polyorb-config
