#
# powerpc_aix_4.2.mk - make variables and rules specific to AIX 4.2 on 
#                      PowerPC.
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
# INSTALL         = cp         # AIX does not have -p
# or use installbsd
INSTALL_USER  = `id -un`
INSTALL_GROUP = `id -gn`
INSTALL       = installbsd -c -o $(INSTALL_USER) -g $(INSTALL_GROUP)

CMAKEDEPEND     = $(TOP)/$(BINDIR)/omkdepend -D_AIX
CXXMAKEDEPEND   = $(TOP)/$(BINDIR)/omkdepend -D__cplusplus -D_AIX

############################################################################
# Using xlC_r                                                              #
############################################################################

CXX             = xlC_r
CXXDEBUGFLAGS   = -O
CXXLINK		= xlC_r

# Use C Set++ to compile your C source.
#
CC               = xlC_r
CLINK           = xlC_r


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
               $(patsubst %,$(LibSharedSearchPattern),omniDynamic27) \
               $(OMNITHREAD_LIB) $(SOCKET_LIB)
lib_depend := $(patsubst %,$(LibSharedPattern),omniORB27) \
              $(patsubst %,$(LibSharedPattern),omniDynamic27)
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


define CXXExecutable
(set -x; \
 $(RM) $@; \
 $(CXXLINK) -o $@ $(CXXLINKOPTIONS) $(IMPORT_LIBRARY_FLAGS) \
    $(filter-out $(LibSharedSuffixPattern), $(filter-out $(LibSuffixPattern),$^)) $$libs; \
)
endef

define CExecutable
(set -x; \
 $(RM) $@; \
 $(CLINK) -o $@ $(CLINKOPTIONS) $(IMPORT_LIBRARY_FLAGS) \
    $(filter-out $(LibSharedSuffixPattern), $(filter-out $(LibSuffixPattern),$^)) $$libs; \
)
endef

# Default location of the omniORB2 configuration file [falls back to this if
# the environment variable OMNIORB_CONFIG is not set] :

OMNIORB_CONFIG_DEFAULT_LOCATION = \"/etc/omniORB.cfg\"

# Default directory for the omniNames log files.
OMNINAMES_LOG_DEFAULT_LOCATION = \"/var/omninames\"
