#
# powerpc_aix_4.2_egcs.mk - make variables and rules specific to AIX 4.2 on 
#                           PowerPC with egcs
#

AIX = 1
PowerPCProcessor = 1

#
# Include general unix things
#

include $(THIS_IMPORT_TREE)/mk/unix.mk

#
# C preprocessor macro definitions for this architecture
#

IMPORT_CPPFLAGS = -D__aix__ -D__powerpc__ -D__OSVERSION__=4

#
# Standard programs
#

AR              = ar cq
RANLIB		= ranlib
MKDIRHIER	= /usr/bin/X11/mkdirhier
INSTALL         = cp -p 


CXXMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D__cplusplus

# Update these variables to point to the location of your egcs installation.
CXX             = /usr/local/bin/g++
CXXLINK	        = /usr/local/bin/g++
CC              = /usr/local/bin/gcc
CLINK           = /usr/local/bin/gcc

CXXDEBUGFLAGS   = 
 
CXXLINKOPTIONS	+= -mthreads

CMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D__GNUC__

# Name all static libraries with -ar.a suffix.
LibPattern = lib%-ar.a
LibSuffixPattern = %-ar.a
LibSearchPattern = -l%-ar

# Name all shared libraries with .a suffix
LibSharedPattern = lib%.a
LibSharedSuffixPattern = %.a
LibSharedSearchPattern = -l%
#
# CORBA stuff
#

# For the moment, gatekeeper feature is disabled with shared library.
# Override the defaults set in unix.mk
#
#omniORB2GatekeeperImplementation = OMNIORB2_TCPWRAPGK
omniORB2GatekeeperImplementation = NO_IMPL
#
# Notice that the version number 2.7 is hardwired in OMNIORB2_LIB.
#
OMNIORB2_LIB = $(patsubst %,$(LibSharedSearchPattern),omniORB27) \
                $(OMNITHREAD_LIB) $(SOCKET_LIB)
lib_depend := $(patsubst %,$(LibSharedPattern),omniORB27)
OMNIORB2_LIB_DEPEND1 := $(GENERATE_LIB_DEPEND)
OMNIORB2_LIB_DEPEND = $(OMNIORB2_LIB_DEPEND1) $(OMNITHREAD_LIB_DEPEND)

OMNIORB2_LC_LIB = $(patsubst %,$(LibSharedSearchPattern),omniLC2)

CorbaImplementation = OMNIORB2

#
# OMNI thread stuff
#
ThreadSystem = Posix

OMNITHREAD_POSIX_CPPFLAGS = -DNoNanoSleep -DPthreadDraftVersion=8
OMNITHREAD_CPPFLAGS = -I$(TOP)/include -D_REENTRANT -D_THREAD_SAFE
OMNITHREAD_LIB = -lomnithread2 -lpthreads
OMNITHREAD_STATIC_LIB = -lomnithread-ar -lpthreads-ar


# Default location of the omniORB2 configuration file [falls back to this if
# the environment variable OMNIORB_CONFIG is not set] :

OMNIORB_CONFIG_DEFAULT_LOCATION = \"/etc/omniORB.cfg\"

# Default directory for the omniNames log files.
OMNINAMES_LOG_DEFAULT_LOCATION = \"/var/omninames\"
