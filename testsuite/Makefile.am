AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support

corba_dir = corba
dsa_dir   = acats
moma_dir  =

giop_dir  =
soap_dir  =
srp_dir   =

SUBDIRS = utils core @APPLI_DIRS@

CLEANFILES = config-stamp

run_tests:
	utils/test_driver -full $(srcdir)/scenarios -output file
