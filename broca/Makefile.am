AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support
SUBDIRS = idlac adasockets src

autoconf::
	aclocal -I support && autoconf && automake
	cd adasockets && (aclocal -I ../support && autoconf && automake)

CLEANFILES = config-stamp
