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

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) polyorb-config $(DESTDIR)$(exec_prefix)/bin/polyorb-config
