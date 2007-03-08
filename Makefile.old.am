AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support

corba_dir      = idls cos
dsa_dir        =
moma_dir       =
web_common_dir =

giop_dir       =
soap_dir       =
srp_dir        =

SUBDIRS = support compilers src @APPLI_DIRS@ @PROTO_DIRS@ @LIBS_DIRS@  tools

CLEANFILES = config-stamp
DISTCLEANFILES = support/adacompiler support/native-adacompiler support/linker polyorb-config

bin_SCRIPTS = polyorb-config

depend:
	cd src && ${MAKE} depend

run_tests:
	cd testsuite && ${MAKE} run_tests
