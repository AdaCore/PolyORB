AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support
SUBDIRS = idlac adasockets src

autoconf::
	aclocal -I support && autoconf && automake
	cd adasockets && (aclocal -I ../support && autoconf && automake)

idlac/configure:
	$(MAKE) autoconf

config-stamp:
	./configure
	touch config-stamp

all-local: config-stamp

CLEANFILES = config-stamp
