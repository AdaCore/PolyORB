AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support

if ENABLE_CORBA
SUBDIRS = compilers src idls cos examples
else
SUBDIRS = compilers src examples
endif

CLEANFILES = config-stamp

depend:
	cd src && ${MAKE} depend
