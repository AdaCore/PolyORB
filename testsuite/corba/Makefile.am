AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support

corba_dir =
dsa_dir   =
moma_dir  =

giop_dir  =
soap_dir  =
srp_dir   =

SUBDIRS = \
	all_exceptions \
	benchs \
	code_sets \
	cos \
	domainmanager \
	harness \
	local \
	location_forwarding \
	orb_init \
	portableinterceptor \
	portableserver \
	rtcorba

CLEANFILES = config-stamp

depend:
	cd src && ${MAKE} depend
