#
# x86_ets.mk - make variables and rules specific to Phar Lap Software
#              Realtime ETS kernel
#

ETSKernel = 1
x86Processor = 1

# Define EmbeddedSystem causes the build process to only build
#    src/lib.
#
EmbeddedSystem = 1

HOSTBINDIR = bin/x86_win32
BINDIR = bin/x86_win32_ets
LIBDIR = lib/x86_win32_ets

#
# Include general win32 things
#

include $(THIS_IMPORT_TREE)/mk/win32.mk


# Override settings in win32.mk.
#  Use static version of the runtime libraries.

ifdef BuildDebugBinary

CXXLINKOPTIONS = $(MSVC_STATICLIB_CXXLINKDEBUGOPTIONS)
CXXDEBUGFLAGS  = 
CXXOPTIONS     = $(MSVC_STATICLIB_CXXDEBUGFLAGS)
CLINKOPTIONS   = $(MSVC_STATICLIB_CLINKDEBUGOPTIONS)
CDEBUGFLAGS    = $(MSVC_STATICLIB_CDEBUGFLAGS)

else

CXXLINKOPTIONS = $(MSVC_STATICLIB_CXXLINKNODEBUGOPTIONS)
CXXDEBUGFLAGS  = -O2
CXXOPTIONS     = $(MSVC_STATICLIB_CXXNODEBUGFLAGS)
CLINKOPTIONS   = $(MSVC_STATICLIB_CLINKNODEBUGOPTIONS)
CDEBUGFLAGS    = -O2
COPTIONS       = $(MSVC_STATICLIB_CNODEBUGFLAGS)

endif


# define _WINSTATIC to build all ETS applications with the static version of
# the runtime libraries.
#
IMPORT_CPPFLAGS += -D__x86__ -D_WINSTATIC -D__ETS_KERNEL__

# Default location of the omniORB2 configuration file.
#
CONFIG_DEFAULT_LOCATION = C:\\OMNIORB.CFG
