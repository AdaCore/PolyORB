AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I support
SUBDIRS = idlac $(ADASOCKETSDIR) src examples cos
CLEANFILES = config-stamp

if HAS_ADASOCKETS
ADASOCKETSDIR =
else
ADASOCKETSDIR = adasockets
endif
