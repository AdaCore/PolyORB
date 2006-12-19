corba_dir = corba
dsa_dir   = acats
moma_dir  =

giop_dir  =
soap_dir  =
srp_dir   =

SUBDIRS = utils core @APPLI_DIRS@

run_tests:
if HAVE_SSL
	openssl genrsa -out privkey.pem
	openssl req -new -x509 -key privkey.pem -out cert.pem -config $(top_srcdir)/testsuite/ssl-cert.conf
endif

	utils/test_driver -full $(srcdir)/scenarios -output file -config $(srcdir)
