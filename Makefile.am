#
# $Id$
#

AUTOMAKE_OPTIONS = foreign no-dependencies
SUBDIRS = Dist Garlic
ACLOCAL_AMFLAGS = -I support

make-check-stamp:
	@$(MAKE) -v -f /dev/null 2>&1 | grep GNU > /dev/null 2>&1 || \
	(echo; echo; \
	 echo WARNING: you are not using GNU make. If you encounter problems; \
	 echo while building GLADE, please install and use GNU make,; \
	 echo available freely from any GNU mirror.; \
	 echo; echo) >&2
	@touch make-check-stamp

Makefile: make-check-stamp

CLEANFILES = make-check-stamp