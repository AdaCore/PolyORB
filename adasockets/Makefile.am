AUTOMAKE_OPTIONS = foreign no-dependencies
SUBDIRS = src
ACLOCAL_AMFLAGS = -I ../support
AUTOHEADER_AMFLAGS = -l ../support

autoconf:
	aclocal -I support && autoconf && automake
