AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support
SUBDIRS = compilers src idls cos examples
CLEANFILES = config-stamp

depend:
	cd src && ${MAKE} depend
