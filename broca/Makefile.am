AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support
SUBDIRS = idlac idls $(ADASOCKETSDIR) src cos examples
CLEANFILES = config-stamp

if HAS_ADASOCKETS
ADASOCKETSDIR =
else
ADASOCKETSDIR = adasockets
endif
