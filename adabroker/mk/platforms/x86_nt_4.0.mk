#
# x86_nt_4.0.mk - make variables and rules specific to Windows NT 4.0.
#

WindowsNT = 1
x86Processor = 1

BINDIR = bin/x86_win32
LIBDIR = lib/x86_win32

#
# Include general win32 things
#

include $(THIS_IMPORT_TREE)/mk/win32.mk



IMPORT_CPPFLAGS += -D__x86__ -D__NT__ -D__OSVERSION__=4


# Default directory for the omniNames log files.
OMNINAMES_LOG_DEFAULT_LOCATION = C:\\temp

