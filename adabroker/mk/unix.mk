#
# Standard make variables and rules for all UNIX platforms.
#

UnixPlatform = 1

#
# Any recursively-expanded variable set here can be overridden _afterwards_ by
# a platform-specific mk file which includes this one.
#

#
# Standard unix programs - note that GNU make already defines some of
# these such as AR, RM, etc (see section 10.3 of the GNU make manual).
#

RANLIB		= ranlib
MKDIRHIER	= mkdirhier
INSTALL		= installbsd -c
INSTLIBFLAGS	= -m 0644
INSTEXEFLAGS	= -m 0755
CP		= cp
MV		= mv -f
CPP		= /lib/cpp


#
# Replacements for implicit rules
#

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

%.o: %.cc
	$(CXX) -c $(CXXFLAGS) -o $@ $<


#
# General rules for cleaning.
#

define CleanRule
$(RM) *.o *.a *.class
endef

# XXX VeryCleanRule should delete Java stubs too.

define VeryCleanRule
$(RM) *.d
$(RM) $(CORBA_STUB_FILES)
endef


#
# Patterns for various file types
#

LibPattern = lib%.a
LibSuffixPattern = %.a
LibSearchPattern = -l%
BinPattern = %
TclScriptPattern = %


#
# Stuff to generate statically-linked libraries.
#

define StaticLinkLibrary
(set -x; \
 $(RM) $@; \
 $(AR) $@ $^; \
 $(RANLIB) $@; \
)
endef

ifdef EXPORT_TREE
define ExportLibrary
(dir="$(EXPORT_TREE)/$(LIBDIR)"; \
 files="$^"; \
 for file in $$files; do \
   $(ExportFileToDir); \
 done; \
)
endef
endif


#
# Stuff to generate executable binaries.
#
# These rules are used like this
#
# target: objs lib_depends
#         @(libs="libs"; $(...Executable))
#
# The command we want to generate is like this
#
# linker -o target ... objs libs
# i.e. we need to filter out the lib_depends from the command
#

IMPORT_LIBRARY_FLAGS = $(patsubst %,-L%,$(IMPORT_LIBRARY_DIRS))

define CXXExecutable
(set -x; \
 $(RM) $@; \
 $(CXXLINK) -o $@ $(CXXLINKOPTIONS) $(IMPORT_LIBRARY_FLAGS) \
    $(filter-out $(LibSuffixPattern),$^) $$libs; \
)
endef

define CExecutable
(set -x; \
 $(RM) $@; \
 $(CLINK) -o $@ $(CLINKOPTIONS) $(IMPORT_LIBRARY_FLAGS) \
    $(filter-out $(LibSuffixPattern),$^) $$libs; \
)
endef

ifdef EXPORT_TREE
define ExportExecutable
(dir="$(EXPORT_TREE)/$(BINDIR)"; \
 files="$^"; \
 for file in $$files; do \
   $(ExportExecutableFileToDir); \
 done; \
)
endef
endif


#
# CORBA stuff
#

lib_depend := $(patsubst %,$(LibPattern),omniORB2)
omniORB_lib_depend := $(GENERATE_LIB_DEPEND)
lib_depend := $(patsubst %,$(LibPattern),omniDynamic2)
omniDynamic_lib_depend := $(GENERATE_LIB_DEPEND)


OMNIORB2_IDL_ONLY = omniidl2
OMNIORB2_IDL_ANY_FLAGS = -a
OMNIORB2_IDL = $(OMNIORB2_IDL_ONLY) $(OMNIORB2_IDL_ANY_FLAGS)
OMNIORB2_CPPFLAGS = -D__OMNIORB2__ -I$(CORBA_STUB_DIR) $(OMNITHREAD_CPPFLAGS)

OMNIORB2_LIB = $(patsubst %,$(LibSearchPattern),omniORB2) \
		$(patsubst %,$(LibSearchPattern),omniDynamic2) \
		$(OMNITHREAD_LIB) $(SOCKET_LIB)
OMNIORB2_LIB_NODYN = $(patsubst %,$(LibSearchPattern),omniORB2) \
		$(OMNITHREAD_LIB) $(SOCKET_LIB)

OMNIORB2_LIB_NODYN_DEPEND = $(omniORB_lib_depend) $(OMNITHREAD_LIB_DEPEND)
OMNIORB2_LIB_DEPEND = $(omniORB_lib_depend) $(OMNITHREAD_LIB_DEPEND) \
		$(omniDynamic_lib_depend)

OMNIORB2_STATIC_STUB_OBJS = $(CORBA_INTERFACES:%=$(CORBA_STUB_DIR)/%SK.o)
OMNIORB2_STATIC_STUB_SRCS = $(CORBA_INTERFACES:%=$(CORBA_STUB_DIR)/%SK.cc)
OMNIORB2_DYN_STUB_OBJS = $(CORBA_INTERFACES:%=$(CORBA_STUB_DIR)/%DynSK.o)
OMNIORB2_DYN_STUB_SRCS = $(CORBA_INTERFACES:%=$(CORBA_STUB_DIR)/%DynSK.cc)

OMNIORB2_STUB_SRCS = $(OMNIORB2_STATIC_STUB_SRCS) $(OMNIORB2_DYN_STUB_SRCS)
OMNIORB2_STUB_OBJS = $(OMNIORB2_STATIC_STUB_OBJS) $(OMNIORB2_DYN_STUB_OBJS)

OMNIORB2_STUB_SRC_PATTERN = $(CORBA_STUB_DIR)/%SK.cc
OMNIORB2_STUB_OBJ_PATTERN = $(CORBA_STUB_DIR)/%SK.o
OMNIORB2_DYN_STUB_SRC_PATTERN = $(CORBA_STUB_DIR)/%DynSK.cc
OMNIORB2_DYN_STUB_OBJ_PATTERN = $(CORBA_STUB_DIR)/%DynSK.o
OMNIORB2_STUB_HDR_PATTERN = $(CORBA_STUB_DIR)/%.hh


# omniORB2 access control policy modules

OMNIORB2_DUMMYGK_LIB = $(patsubst %,$(LibSearchPattern),omniGK_stub)
lib_depend := $(patsubst %,$(LibPattern),omniGK_stub)
OMNIORB2_DUMMYGK_LIB_DEPEND := $(GENERATE_LIB_DEPEND)

OMNIORB2_TCPWRAPGK_LIB = $(patsubst %,$(LibSearchPattern),tcpwrapGK)
lib_depend := $(patsubst %,$(LibPattern),tcpwrapGK)
OMNIORB2_TCPWRAPGK_LIB_DEPEND := $(GENERATE_LIB_DEPEND)

omniORB2GatekeeperImplementation = OMNIORB2_DUMMYGK

OMNIORB2_LIB += $($(omniORB2GatekeeperImplementation)_LIB)
OMNIORB2_LIB_NODYN += $($(omniORB2GatekeeperImplementation)_LIB)
OMNIORB2_LIB_DEPEND += $($(omniORB2GatekeeperImplementation)_LIB_DEPEND)
OMNIORB2_LIB_NODYN_DEPEND += $($(omniORB2GatekeeperImplementation)_LIB_DEPEND)


# LifeCycle stuff

OMNIORB2_IDL_LC_FLAGS = -l
OMNIORB2_LC_LIB = $(patsubst %,$(LibSearchPattern),omniLC)

#
# Tcl stuff
#

define TclScriptExecutable
((set -x; $(RM) $@); \
 if [ "$$wish" = "" ]; then \
   wish="$(WISH4)"; \
 fi; \
 case "$$wish" in \
 /*) ;; \
 *) \
   if [ "$(EXPORT_TREE)" != "" ]; then \
     wish="$(EXPORT_TREE)/$(BINDIR)/$$wish"; \
   else \
     wish="./$$wish"; \
   fi ;; \
 esac; \
 echo echo "#!$$wish >$@"; \
 echo "#!$$wish" >$@; \
 echo echo "set auto_path [concat {$$tcllibpath} \$$auto_path] >>$@"; \
 echo "set auto_path [concat {$$tcllibpath} \$$auto_path]" >>$@; \
 echo "cat $< >>$@"; \
 cat $< >>$@; \
 set -x; \
 chmod a+x $@; \
)
endef
