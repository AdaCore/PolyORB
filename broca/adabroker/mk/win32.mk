#
# Standard make variables and rules for all Win32 platforms.
#

Win32Platform = 1


#
# Standard "unix" programs.  Anything here not provided by the GNU-WIN32
# system is likely to need a wrapper around it to perform filename translation.
#
ifndef OpenNTBuildTree

# GNU-WIN32 wrappers

AR = $(TOP)/$(BINDIR)/libwrapper -gnuwin32

CXX = $(TOP)/$(BINDIR)/clwrapper -gnuwin32
CXXMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D__cplusplus -D_MSC_VER
CXXLINK	= $(TOP)/$(BINDIR)/linkwrapper -gnuwin32

CC = $(TOP)/$(BINDIR)/clwrapper -gnuwin32
CMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D_MSC_VER
CLINK = $(TOP)/$(BINDIR)/linkwrapper -gnuwin32

# There is a sort in %System32%/sort.exe and in GNU-WIN32. The shell of
# GNU-WIN32 may pick either one depending on the PATH setup of the user.
# To make sure that the GNU-WIN32 version is picked up, give the pathname
# of sort.

SORT = /bin/sort

else

# OpenNT wrappers

CXX = $(TOP)/$(BINDIR)/clwrapper.exe -opennt
CXXLINK = $(TOP)/$(BINDIR)/linkwrapper.exe -opennt
AR  = $(TOP)/$(BINDIR)/libwrapper.exe -opennt

CXXMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D__cplusplus -D_MSC_VER
CC = $(TOP)/$(BINDIR)/clwrapper.exe -opennt
CMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D_MSC_VER
CLINK = $(TOP)/$(BINDIR)/linkwrapper.exe -opennt

SORT = sort

endif


MKDIRHIER	= mkdir -p

INSTALL		= install -c
INSTLIBFLAGS	= -m 0644
INSTEXEFLAGS	= -m 0755

CP = cp
MV = mv -f


# Use the following set of flags to build and use multithreaded DLLs
#
MSVC_DLL_CXXNODEBUGFLAGS       = -MD -GX
MSVC_DLL_CXXLINKNODEBUGOPTIONS = 
MSVC_DLL_CNODEBUGFLAGS         = -MD
MSVC_DLL_CLINKNODEBUGOPTIONS   = 
#
MSVC_DLL_CXXDEBUGFLAGS         = -MDd -GX -Z7 -Od 
MSVC_DLL_CXXLINKDEBUGOPTIONS   = -debug -PDB:NONE
MSVC_DLL_CDEBUGFLAGS           = -MDd -Z7 -Od
MSVC_DLL_CLINKDEBUGOPTIONS     = -debug -PDB:NONE
#
# Or
#
# Use the following set of flags to build and use multithread static libraries
#
MSVC_STATICLIB_CXXNODEBUGFLAGS       = -MT -GX
MSVC_STATICLIB_CXXLINKNODEBUGOPTIONS = 
MSVC_STATICLIB_CNODEBUGFLAGS         = -MT
MSVC_STATICLIB_CLINKNODEBUGOPTIONS   = 

MSVC_STATICLIB_CXXDEBUGFLAGS         = -MTd -GX -Z7 -Od 
MSVC_STATICLIB_CXXLINKDEBUGOPTIONS   = -debug -PDB:NONE
MSVC_STATICLIB_CDEBUGFLAGS           = -MTd -Z7 -Od
MSVC_STATICLIB_CLINKDEBUGOPTIONS     = -debug -PDB:NONE


ifdef BuildDebugBinary

CXXLINKOPTIONS = $(MSVC_DLL_CXXLINKDEBUGOPTIONS)
CXXDEBUGFLAGS  = 
CXXOPTIONS     = $(MSVC_DLL_CXXDEBUGFLAGS)
CLINKOPTIONS   = $(MSVC_DLL_CLINKDEBUGOPTIONS)
CDEBUGFLAGS    = $(MSVC_DLL_CDEBUGFLAGS)

else

CXXLINKOPTIONS = $(MSVC_DLL_CXXLINKNODEBUGOPTIONS)
CXXDEBUGFLAGS  = -O2
CXXOPTIONS     = $(MSVC_DLL_CXXNODEBUGFLAGS)
CLINKOPTIONS   = $(MSVC_DLL_CLINKNODEBUGOPTIONS)
CDEBUGFLAGS    = -O2
COPTIONS       = $(MSVC_DLL_CNODEBUGFLAGS)

endif

IMPORT_CPPFLAGS += -D__WIN32__

SOCKET_LIB = wsock32.lib


#
# Replacements for implicit rules
#

%.o: %.c
	$(CC) -c $(CFLAGS) -Fo$@ $<

%.o: %.cc
	$(CXX) -c $(CXXFLAGS) -Fo$@ $<


#
# General rule for cleaning.
#

define CleanRule
$(RM) *.o *.lib
endef

define VeryCleanRule
$(RM) *.d
$(RM) $(CORBA_STUB_FILES)
endef


#
# Patterns for various file types
#

LibNoDebugPattern = %.lib
LibDebugPattern = %d.lib
DLLNoDebugPattern = %_rt.lib
DLLDebugPattern = %_rtd.lib
LibNoDebugSearchPattern = %.lib
LibDebugSearchPattern = %d.lib
DLLNoDebugSearchPattern = %_rt.lib
DLLDebugSearchPattern = %_rtd.lib


ifndef BuildDebugBinary

LibPattern = $(LibNoDebugPattern)
DLLPattern = $(DLLNoDebugPattern)
LibSearchPattern = $(LibNoDebugSearchPattern)
DLLSearchPattern = $(DLLNoDebugSearchPattern)

else

LibPattern = $(LibDebugPattern)
DLLPattern = $(DLLDebugPattern)
LibSearchPattern = $(LibDebugSearchPattern)
DLLSearchPattern = $(DLLDebugSearchPattern)

endif

BinPattern = %.exe


#
# Stuff to generate statically-linked libraries.
#

define StaticLinkLibrary
(set -x; \
 $(RM) $@; \
 $(AR) $@ $^; \
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

IMPORT_LIBRARY_FLAGS = $(patsubst %,-libpath:%/$(LIBDIR),$(IMPORT_TREES))

define CXXExecutable
(set -x; \
 $(RM) $@; \
 $(CXXLINK) -out:$@ $(CXXLINKOPTIONS) $(IMPORT_LIBRARY_FLAGS) $^ $$libs; \
)
endef

define CExecutable
(set -x; \
 $(RM) $@; \
 $(CLINK) -out:$@ $(CLINKOPTIONS) $(IMPORT_LIBRARY_FLAGS) $^ $$libs; \
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

# Note that the DLL version is being used, so link to omniorb2_rt.lib

lib_depend := $(patsubst %,$(DLLPattern),omniORB271)
omniORB_lib_depend := $(GENERATE_LIB_DEPEND)
lib_depend := $(patsubst %,$(DLLPattern),omniDynamic271)
omniDynamic_lib_depend := $(GENERATE_LIB_DEPEND)


OMNIORB2_IDL_ONLY = omniidl2.exe -h .hh -s SK.cc
OMNIORB2_IDL_ANY_FLAGS = -a
OMNIORB2_IDL = $(OMNIORB2_IDL_ONLY) $(OMNIORB2_IDL_ANY_FLAGS)
OMNIORB2_CPPFLAGS = -D__OMNIORB2__ -I$(CORBA_STUB_DIR) $(OMNITHREAD_CPPFLAGS)

OMNIORB2_LIB = $(patsubst %,$(DLLSearchPattern),omniORB271) \
		$(patsubst %,$(DLLSearchPattern),omniDynamic271) \
		$(OMNITHREAD_LIB) wsock32.lib advapi32.lib
OMNIORB2_LIB_NODYN = $(patsubst %,$(DLLSearchPattern),omniORB271) \
		$(OMNITHREAD_LIB) wsock32.lib advapi32.lib

OMNIORB2_LIB_NODYN_DEPEND := $(omniORB_lib_depend) $(OMNITHREAD_LIB_DEPEND)
OMNIORB2_LIB_DEPEND := $(omniORB_lib_depend) $(OMNITHREAD_LIB_DEPEND) \
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


# LifeCycle stuff

OMNIORB2_IDL_LC_FLAGS = -l
OMNIORB2_LC_LIB = $(patsubst %,$(DLLSearchPattern),omniLC22)

CorbaImplementation = OMNIORB2

#
# OMNI thread stuff
#
# Note that the DLL version is being used, so link to omnithread_rt.lib

ThreadSystem = NT
OMNITHREAD_LIB = $(patsubst %,$(DLLSearchPattern),omnithread2)
lib_depend := $(patsubst %,$(DLLPattern),omnithread2)
OMNITHREAD_LIB_DEPEND := $(GENERATE_LIB_DEPEND)
