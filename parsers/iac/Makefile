# Generated automatically from Makefile.in by configure.
# Makefile.in generated automatically by automake 1.4-p6 from Makefile.am

# Copyright (C) 1994, 1995-8, 1999, 2001 Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.



SHELL = /bin/sh

srcdir = .
common_dir= ../common_files
top_srcdir = ../..
prefix = /home/pautet/p4/polyorb
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datadir = ${prefix}/share
sysconfdir = ${prefix}/etc
sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var
libdir = ${exec_prefix}/lib
infodir = ${prefix}/info
mandir = ${prefix}/man
includedir = ${prefix}/include
oldincludedir = /usr/include

includes = -I. -I$(srcdir) -I../common_files

DESTDIR =

pkgdatadir = $(datadir)/polyorb
pkglibdir = $(libdir)/polyorb
pkgincludedir = $(includedir)/polyorb

top_builddir = ../..

ACLOCAL = aclocal-1.4
AUTOCONF = autoconf
AUTOMAKE = automake-1.4
AUTOHEADER = autoheader

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL} $(AM_INSTALL_PROGRAM_FLAGS)
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_SCRIPT = ${INSTALL_PROGRAM}
transform = s,x,x,

NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
build_alias = i686-pc-linux-gnu
build_triplet = i686-pc-linux-gnu
host_alias = i686-pc-linux-gnu
host_triplet = i686-pc-linux-gnu
target_alias = i686-pc-linux-gnu
target_triplet = i686-pc-linux-gnu
ADA = gcc
ADA_FOR_TARGET = gcc
APPLI_DIRS =  $(dsa_dir) $(corba_dir)
APPLI_EXES = $(poly_exe)  $(dsa_exe) $(corba_exe)
APPLI_INCS =  $(dsa_inc) $(corba_inc)
APPLI_LIBS =  $(dsa_lib) $(corba_lib)
APPLI_LIST =  dsa  corba 
AS = @AS@
AWK = awk
BARGS = -E
LARGS = -g
CC = gcc
CC_FOR_TARGET = gcc
CXXCPP = g++ -E
CXXCPPFLAGS = -C -x c -ansi
DLLTOOL = @DLLTOOL@
DOC_DIR = 
ECHO = echo
EXEEXT = 
FIND = find
GNAMAKE_FOR_TARGET = 
GNATCHOP = gnatchop
GNATLS = @GNATLS@
GNATMAKE = gnatmake
GNATMAKE_FOR_TARGET = gnatmake
GREP = @GREP@
HAVE_GNAT_OS_LIB_CLOSE_WITH_STATUS = @HAVE_GNAT_OS_LIB_CLOSE_WITH_STATUS@
HAVE_GNAT_SOCKETS_COPY = 
IDLAC = idlac
LIBS_DIRS = 
LIBS_EXES = 
LIBS_INCS = 
LIBS_LIBS = 
LIBS_LIST = 
LIBTOOL = $(SHELL) $(top_builddir)/libtool
LIBVERSIONINFO = 0:0:0
LN_S = ln -s
MAKEINFO = makeinfo
MISS_GNAT_OS_LIB_CLOSE_WITH_STATUS = @MISS_GNAT_OS_LIB_CLOSE_WITH_STATUS@
MISS_GNAT_SOCKETS_COPY = --  
OBJDUMP = @OBJDUMP@
OBJEXT = o
PACKAGE = polyorb
POLYORB_MAJOR_VERSION = 0
POLYORB_MINOR_VERSION = 1
PROTO_DIRS =  $(giop_dir)
PROTO_INCS =  $(giop_inc)
PROTO_LIBS =  $(giop_lib)
PROTO_LIST =  giop 
PYTHON = python
RANLIB = ranlib
RM = rm
SED = sed
SERVICE_DIRS =  $(naming_dir)
SERVICE_EXES =  $(naming_exe)
SERVICE_INCS =  $(naming_inc)
SERVICE_LIST =  naming 
STRIP = strip
TEXI2HTML = 
VERSION = 0.1
XMLADA_INCS = 
XMLADA_LIBS = 

SUBDIRS = 

GNATFLAGS = -g -O2 -gnat05 -gnatfy -gnatwae -gnatoa -fstack-check -gnatg

PROG = iac

PROG_FILES = \
frontend-nodes.ada \
backend-be_ada-nodes.ada

CLEANFILES = *.o *.ali b~*.ads b~*.adb *~ backend-be_ada-nodes.ad? \
             frontend-nodes.ad?

DISTCLEANFILES = $(PROG) mknodes
mkinstalldirs = $(SHELL) $(top_srcdir)/support/mkinstalldirs
CONFIG_CLEAN_FILES =  platform.ads
DIST_COMMON =  Makefile.am Makefile.in TODO platform.ads.in


DISTFILES = $(DIST_COMMON) $(SOURCES) $(HEADERS) $(TEXINFOS) $(EXTRA_DIST)

TAR = tar
GZIP_ENV = --best
all: all-redirect
.SUFFIXES:

# This directory's subdirectories are mostly independent; you can cd
# into them and run `make' without going through this Makefile.
# To change the values of `make' variables: instead of editing Makefiles,
# (1) if the variable is set in `config.status', edit `config.status'
#     (which will cause the Makefiles to be regenerated when you run `make');
# (2) otherwise, pass the desired values on the `make' command line.



all-recursive install-data-recursive install-exec-recursive \
installdirs-recursive install-recursive uninstall-recursive  \
check-recursive installcheck-recursive info-recursive dvi-recursive:
	@set fnord $(MAKEFLAGS); amf=$$2; \
	dot_seen=no; \
	target=`echo $@ | sed s/-recursive//`; \
	list='$(SUBDIRS)'; for subdir in $$list; do \
	  echo "Making $$target in $$subdir"; \
	  if test "$$subdir" = "."; then \
	    dot_seen=yes; \
	    local_target="$$target-am"; \
	  else \
	    local_target="$$target"; \
	  fi; \
	  (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) $$local_target) \
	   || case "$$amf" in *=*) exit 1;; *k*) fail=yes;; *) exit 1;; esac; \
	done; \
	if test "$$dot_seen" = "no"; then \
	  $(MAKE) $(AM_MAKEFLAGS) "$$target-am" || exit 1; \
	fi; test -z "$$fail"

mostlyclean-recursive clean-recursive distclean-recursive \
maintainer-clean-recursive:
	@set fnord $(MAKEFLAGS); amf=$$2; \
	dot_seen=no; \
	rev=''; list='$(SUBDIRS)'; for subdir in $$list; do \
	  rev="$$subdir $$rev"; \
	  test "$$subdir" != "." || dot_seen=yes; \
	done; \
	test "$$dot_seen" = "no" && rev=". $$rev"; \
	target=`echo $@ | sed s/-recursive//`; \
	for subdir in $$rev; do \
	  echo "Making $$target in $$subdir"; \
	  if test "$$subdir" = "."; then \
	    local_target="$$target-am"; \
	  else \
	    local_target="$$target"; \
	  fi; \
	  (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) $$local_target) \
	   || case "$$amf" in *=*) exit 1;; *k*) fail=yes;; *) exit 1;; esac; \
	done && test -z "$$fail"
tags-recursive:
	list='$(SUBDIRS)'; for subdir in $$list; do \
	  test "$$subdir" = . || (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) tags); \
	done

tags: TAGS

ID: $(HEADERS) $(SOURCES) $(LISP)
	list='$(SOURCES) $(HEADERS)'; \
	unique=`for i in $$list; do echo $$i; done | \
	  awk '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	here=`pwd` && cd $(srcdir) \
	  && mkid -f$$here/ID $$unique $(LISP)

TAGS: tags-recursive $(HEADERS) $(SOURCES)  $(TAGS_DEPENDENCIES) $(LISP)
	tags=; \
	here=`pwd`; \
	list='$(SUBDIRS)'; for subdir in $$list; do \
   if test "$$subdir" = .; then :; else \
	    test -f $$subdir/TAGS && tags="$$tags -i $$here/$$subdir/TAGS"; \
   fi; \
	done; \
	list='$(SOURCES) $(HEADERS)'; \
	unique=`for i in $$list; do echo $$i; done | \
	  awk '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	test -z "$(ETAGS_ARGS)$$unique$(LISP)$$tags" \
	  || (cd $(srcdir) && etags -o $$here/TAGS $(ETAGS_ARGS) $$tags  $$unique $(LISP))

mostlyclean-tags:

clean-tags:

distclean-tags:
	-rm -f TAGS ID

maintainer-clean-tags:

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)

subdir = compilers/iac

distdir: $(DISTFILES)
	here=`cd $(top_builddir) && pwd`; \
	top_distdir=`cd $(top_distdir) && pwd`; \
	distdir=`cd $(distdir) && pwd`; \
	cd $(top_srcdir) \
	  && $(AUTOMAKE) --include-deps --build-dir=$$here --srcdir-name=$(top_srcdir) --output-dir=$$top_distdir --gnu compilers/iac/Makefile
	@for file in $(DISTFILES); do \
	  d=$(srcdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$d/$$file $(distdir)/$$file; \
	  else \
	    test -f $(distdir)/$$file \
	    || ln $$d/$$file $(distdir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(distdir)/$$file || :; \
	  fi; \
	done
	for subdir in $(SUBDIRS); do \
	  if test "$$subdir" = .; then :; else \
	    test -d $(distdir)/$$subdir \
	    || mkdir $(distdir)/$$subdir \
	    || exit 1; \
	    chmod 777 $(distdir)/$$subdir; \
	    (cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) top_distdir=../$(top_distdir) distdir=../$(distdir)/$$subdir distdir) \
	      || exit 1; \
	  fi; \
	done
info-am:
info: info-recursive
dvi-am:
dvi: dvi-recursive
check-am: all-am
check: check-recursive
installcheck-am:
installcheck: installcheck-recursive
install-exec-am: install-exec-local
install-exec: install-exec-recursive

install-data-am:
install-data: install-data-recursive

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am
install: install-recursive
uninstall-am:
uninstall: uninstall-recursive
all-am: Makefile all-local
all-redirect: all-recursive
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) AM_INSTALL_PROGRAM_FLAGS=-s install
installdirs: installdirs-recursive
installdirs-am:


mostlyclean-generic:

clean-generic:
	-test -z "$(CLEANFILES)" || rm -f $(CLEANFILES)

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES)
	-rm -f config.cache config.log stamp-h stamp-h[0-9]*
	-test -z "$(DISTCLEANFILES)" || rm -f $(DISTCLEANFILES)

maintainer-clean-generic:
mostlyclean-am:  mostlyclean-tags mostlyclean-generic

mostlyclean: mostlyclean-recursive

clean-am:  clean-tags clean-generic mostlyclean-am

clean: clean-recursive

distclean-am:  distclean-tags distclean-generic clean-am
	-rm -f libtool

distclean: distclean-recursive

maintainer-clean-am:  maintainer-clean-tags maintainer-clean-generic \
		distclean-am
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."

maintainer-clean: maintainer-clean-recursive

.PHONY: install-data-recursive uninstall-data-recursive \
install-exec-recursive uninstall-exec-recursive installdirs-recursive \
uninstalldirs-recursive all-recursive check-recursive \
installcheck-recursive info-recursive dvi-recursive \
mostlyclean-recursive distclean-recursive clean-recursive \
maintainer-clean-recursive tags tags-recursive mostlyclean-tags \
distclean-tags clean-tags maintainer-clean-tags distdir info-am info \
dvi-am dvi check check-am installcheck-am installcheck \
install-exec-local install-exec-am install-exec install-data-am \
install-data install-am install uninstall-am uninstall all-local \
all-redirect all-am all installdirs-am installdirs mostlyclean-generic \
distclean-generic clean-generic maintainer-clean-generic clean \
mostlyclean distclean maintainer-clean


all-local: $(PROG)

%.ada:%.idl mknodes
	@-./mknodes $< >$@
	gnatchop -w $@
	$(GNATMAKE) -a -m -gnata $(includes) $*.adb\
		-cargs $(GNATFLAGS) -bargs $(BARGS) -largs $(LARGS)

mknodes : force 
	$(GNATMAKE) -a -m -gnata -gnatyo $(includes) $@\
		-cargs $(GNATFLAGS) -bargs $(BARGS) -largs $(LARGS)

$(PROG): force mknodes $(PROG_FILES)
	$(GNATMAKE) -a -m -gnata -gnatyo $(includes) $@\
		-cargs $(GNATFLAGS) -bargs $(BARGS) -largs $(LARGS)

tests: $(PROG)
	@for i in `cat testsuite/MANIFEST | grep -v '^#'`; do \
	  echo $$i; \
	  testsuite/run-test testsuite/$$i; \
	done

force::

install-exec-local:
	$(INSTALL) -d $(DESTDIR)$(exec_prefix)/bin
	$(INSTALL_PROGRAM) $(PROG) $(DESTDIR)$(exec_prefix)/bin/$(PROG)

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
