AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support
SUBDIRS = idlac adasockets src examples cos

autoconf::
	aclocal -I support && autoconf && automake
	cd adasockets && aclocal -I ../support && autoconf && automake && autoheader
	cd idlac && $(MAKE) -f Makefile.am autoconf

CLEANFILES = config-stamp
