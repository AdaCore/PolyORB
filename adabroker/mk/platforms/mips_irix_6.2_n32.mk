#
# sgi_irix_6.2.mk - make variables and rules specific to SGI Irix 6.2
#

IRIX = 1
IRIX_n32 = 1
IndigoProcessor = 1


#
# Include general unix things
#

include $(THIS_IMPORT_TREE)/mk/unix.mk

#
# C preprocessor macro definitions for this architecture
#

IMPORT_CPPFLAGS += -D__mips__ -D__irix__ -D__OSVERSION__=6


#
# Standard programs
#

AR = ar cq
RANLIB = true

MKDIRHIER = mkdirhier
INSTALL   = $(TOP)/bin/scripts/install-sh -c

CPP = 'CC -E'

# The cc/CC version 7.2 (mips)
#
CXX = CC
CXXMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D__SGI_CC -D__cplusplus
CXXDEBUGFLAGS = 
CXXWOFFOPTIONS =  -woff 3303,1110,1182
CXXOPTIONS     =  -n32 -float -ansi -LANG:exceptions=ON $(CXXWOFFOPTIONS)
CXXLINK		= $(CXX)
CXXLINKOPTIONS  = $(CXXDEBUGFLAGS) $(CXXOPTIONS)

CC                = cc
COPTIONS          = -n32
CLINKOPTIONS      = $(COPTIONS)
CMAKEDEPEND       = $(TOP)/$(BINDIR)/omkdepend
CLINK             = $(CC)

#
# OMNI thread stuff
#

Posix_OMNITHREAD_LIB = $(patsubst %,$(LibSearchPattern),omnithread) -lpthread
Posix_OMNITHREAD_CPPFLAGS = -DUsePthread -D_REENTRANT 
OMNITHREAD_POSIX_CPPFLAGS = -DPthreadDraftVersion=10 \
			    -DPthreadSupportThreadPriority

OMNITHREAD_LIB = $($(ThreadSystem)_OMNITHREAD_LIB)
OMNITHREAD_CPPFLAGS = $($(ThreadSystem)_OMNITHREAD_CPPFLAGS)

ThreadSystem = Posix

lib_depend := $(patsubst %,$(LibPattern),omnithread)
OMNITHREAD_LIB_DEPEND := $(GENERATE_LIB_DEPEND)

#
# CORBA stuff
#

omniORB2GatekeeperImplementation = OMNIORB2_TCPWRAPGK
CorbaImplementation = OMNIORB2



# Default location of the omniORB2 configuration file [falls back to this if
# the environment variable OMNIORB_CONFIG is not set] :

OMNIORB_CONFIG_DEFAULT_LOCATION = \"/etc/omniORB.cfg\"

# Default directory for the omniNames log files.
OMNINAMES_LOG_DEFAULT_LOCATION = \"/var/omninames\"
