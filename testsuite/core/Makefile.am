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
	calendar \
	chained_lists \
	dynamic_dict \
	fixed_point \
	initialization \
	naming \
	obj_adapters \
	poa \
	tasking

CLEANFILES = config-stamp

depend:
	cd src && ${MAKE} depend
