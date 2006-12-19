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
top_srcdir = ../../..
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

DESTDIR =

pkgdatadir = $(datadir)/polyorb
pkglibdir = $(libdir)/polyorb
pkgincludedir = $(includedir)/polyorb

top_builddir = ../../..

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
CC = gcc
CC_FOR_TARGET = gcc
CXXCPP = g++ -E
CXXCPPFLAGS = -C -x c -ansi
DLLTOOL = @DLLTOOL@
DOC_DIR = 
DVIPS = dvips
ECHO = echo
EXEEXT = 
FIND = find
GNAMAKE_FOR_TARGET = 
GNATCHOP = gnatchop
GNATFLAGS = -g -O2 -gnatfy -gnatwae -gnatoa -fstack-check
GNATLS = @GNATLS@
GNATMAKE = gnatmake
GNATMAKE_FOR_TARGET = gnatmake
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
TEX = tex
TEXI2HTML = 
VERSION = 0.1
XMLADA_INCS = 
XMLADA_LIBS = 
mkinstalldirs = $(SHELL) $(top_srcdir)/support/mkinstalldirs
CONFIG_CLEAN_FILES = 
DIST_COMMON =  Makefile.am Makefile.in


DISTFILES = $(DIST_COMMON) $(SOURCES) $(HEADERS) $(TEXINFOS) $(EXTRA_DIST)

TAR = tar
GZIP_ENV = --best
all: all-redirect
.SUFFIXES:

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)

subdir = compilers/iac/testsuite

distdir: $(DISTFILES)
	here=`cd $(top_builddir) && pwd`; \
	top_distdir=`cd $(top_distdir) && pwd`; \
	distdir=`cd $(distdir) && pwd`; \
	cd $(top_srcdir) \
	  && $(AUTOMAKE) --include-deps --build-dir=$$here --srcdir-name=$(top_srcdir) --output-dir=$$top_distdir --gnu compilers/iac/testsuite/Makefile
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
info-am:
info: info-am
dvi-am:
dvi: dvi-am
check-am: all-am
check: check-am
installcheck-am:
installcheck: installcheck-am
install-exec-am:
install-exec: install-exec-am

install-data-am:
install-data: install-data-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am
install: install-am
uninstall-am:
uninstall: uninstall-am
all-am: Makefile
all-redirect: all-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) AM_INSTALL_PROGRAM_FLAGS=-s install
installdirs:


mostlyclean-generic:

clean-generic:

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES)
	-rm -f config.cache config.log stamp-h stamp-h[0-9]*

maintainer-clean-generic:
mostlyclean-am:  mostlyclean-generic

mostlyclean: mostlyclean-am

clean-am:  clean-generic mostlyclean-am

clean: clean-am

distclean-am:  distclean-generic clean-am
	-rm -f libtool

distclean: distclean-am

maintainer-clean-am:  maintainer-clean-generic distclean-am
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."

maintainer-clean: maintainer-clean-am

.PHONY: tags distdir info-am info dvi-am dvi check check-am \
installcheck-am installcheck install-exec-am install-exec \
install-data-am install-data install-am install uninstall-am uninstall \
all-redirect all-am all installdirs mostlyclean-generic \
distclean-generic clean-generic maintainer-clean-generic clean \
mostlyclean distclean maintainer-clean


all:
	@for i in `cat MANIFEST | grep -v '^#'`; do \
	  ./run-test.sh $$i; \
	done

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
