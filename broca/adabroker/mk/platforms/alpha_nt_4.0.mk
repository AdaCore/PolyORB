#
# alpha_nt_4.0.mk - make variables and rules specific to Windows NT 4.0
#                   with Alpha processor

WindowsNT = 1
AlphaProcessor = 1

BINDIR = bin/alpha_win32
LIBDIR = lib/alpha_win32

# Use the build environment under OpenNT
OpenNTBuildTree = 1

SHELL=//D/OpenNT/bin/sh

#
# Include general win32 things
#

include $(THIS_IMPORT_TREE)/mk/win32.mk



IMPORT_CPPFLAGS += -D__alpha__ -D__NT__ -D__OSVERSION__=4


# Default directory for the omniNames log files.
OMNINAMES_LOG_DEFAULT_LOCATION = C:\\temp
