AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support

corba_dir =
dsa_dir   =
moma_dir  =

giop_dir  =
soap_dir  =
srp_dir   =

SUBDIRS = \
	any \
	chained_lists \
	dynamic_dict \
	fixed_point \
	initialization \
	naming \
	obj_adapters \
	poa \
	random \
	sync_policies \
	tasking \
	uri_encoding

CLEANFILES = config-stamp

depend:
	cd src && ${MAKE} depend
