AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support

corba_dir =
dsa_dir   =
moma_dir  =

giop_dir  =
soap_dir  =
srp_dir   =

SUBDIRS = utils core

CLEANFILES = config-stamp

run_tests:
	utils/test_driver -full scenarios
