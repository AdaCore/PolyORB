#
# x86_sosV_5.5.mk - make variables and rules specific to Solaris 2.5.
#

SunOS = 1
x86Processor = 1


#
# Include general unix things
#

include $(THIS_IMPORT_TREE)/mk/unix.mk


#
# C preprocessor macro definitions for this architecture
#

IMPORT_CPPFLAGS += -D__x86__ -D__sunos__ -D__OSVERSION__=5


#
# Standard programs
#

AR = ar cq

MKDIRHIER = mkdirhier

CPP = /usr/ccs/lib/cpp

CXX = CC
CXXMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D__SUNPRO_CC -D__cplusplus
CXXDEBUGFLAGS = -O2 -fsimple
CXXMTFLAG     = -mt

CXXLINK		= $(CXX)
CXXLINKOPTIONS  = $(CXXDEBUGFLAGS) $(CXXOPTIONS)

# CXXLINKOPTIONS += $(patsubst %,-R %,$(IMPORT_LIBRARY_DIRS))
# Note: the -R linker option in CXXLINKOPTIONS instruct the Sun linker to
# record the pathname of the shared libraries in the executable.

#
# To use gcc uncomment the following lines:
#
#CPP = gcc
#
#CXX = g++
#CXXMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D__cplusplus -D__GNUG__ -D__GNUC__
#CXXDEBUGFLAGS = 
#CXXOPTIONS    =  -fhandle-exceptions -Wall -Wno-unused
#CXXMTFLAG     =
#
#CXXLINK		= $(CXX)
#CXXLINKOPTIONS  = $(CXXDEBUGFLAGS) $(CXXOPTIONS)
#
# CXXLINKOPTIONS += $(patsubst %,-R %,$(IMPORT_LIBRARY_DIRS))
# Note: the -R linker option in CXXLINKOPTIONS instruct the Sun linker to
# record the pathname of the shared libraries in the executable.

CC                = gcc
CMAKEDEPEND       = $(TOP)/$(BINDIR)/omkdepend -D__GNUC__
CDEBUGFLAGS       = -O
COPTIONS	  = -fpcc-struct-return

CLINK             = $(CC)


#
# Socket library
#

SOCKET_LIB = -lsocket -lnsl
THREAD_LIB = -lthread $(CXXMTFLAG)


#
# CORBA stuff
#

omniORB2GatekeeperImplementation = OMNIORB2_TCPWRAPGK
CorbaImplementation = OMNIORB2

#
# OMNI thread stuff
#

Solaris_OMNITHREAD_LIB = $(patsubst %,$(LibSearchPattern),omnithread) \
			 -lthread -lposix4 $(CXXMTFLAG)
Solaris_OMNITHREAD_CPPFLAGS = -D_REENTRANT $(CXXMTFLAG)

Posix_OMNITHREAD_LIB = $(patsubst %,$(LibSearchPattern),omnithread) -lpthread \
		 -lposix4 $(CXXMTFLAG)
Posix_OMNITHREAD_CPPFLAGS = -DUsePthread -D_REENTRANT $(CXXMTFLAG)
OMNITHREAD_POSIX_CPPFLAGS = -DPthreadDraftVersion=10 \
			    -DPthreadSupportThreadPriority

OMNITHREAD_LIB = $($(ThreadSystem)_OMNITHREAD_LIB)
OMNITHREAD_CPPFLAGS = $($(ThreadSystem)_OMNITHREAD_CPPFLAGS)

ThreadSystem = Posix

lib_depend := $(patsubst %,$(LibPattern),omnithread)
OMNITHREAD_LIB_DEPEND := $(GENERATE_LIB_DEPEND)


# Default location of the omniORB2 configuration file [falls back to this if
# the environment variable OMNIORB_CONFIG is not set] :

OMNIORB_CONFIG_DEFAULT_LOCATION = \"/etc/omniORB.cfg\"

# Default directory for the omniNames log files.
OMNINAMES_LOG_DEFAULT_LOCATION = \"/var/omninames\"
