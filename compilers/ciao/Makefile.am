include $(srcdir)/../Makefile.common

ASIS_INCLUDES=	$(HOME)/gnat-3.15w/asis
ASIS_LIBS=	-L$(HOME)/gnat-3.15w/asis -lasis

all-local: ciao

ciao: force $(TREE_FILES)
	cd ../idlac && $(MAKE) all
	$(GNATMAKE) -m \
	-I$(srcdir) \
	-I$(srcdir)/../idlac \
	-I../idlac \
	-I$(ASIS_INCLUDES) \
	-o $@ ciao-driver.adb \
	-cargs $(GNATFLAGS) \
	-bargs $(BARGS) \
	-largs $(ASIS_LIBS)

CLEANFILES = *.o *.ali b~*.ads b~*.adb

DISTCLEANFILES = $(TREE_FILES) ciao

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) idlac $(DESTDIR)$(exec_prefix)/bin/idlac
