#
# m68k_nextstep_3.3.mk - make variables and rules specific to m68k nextstep 3.3
#

NextStep = 1
m68kProcessor = 1


#
# Include general unix things
#

include $(THIS_IMPORT_TREE)/mk/unix.mk


#
# Standard programs
#

AR = ar cq
MKDIRHIER = mkdirs

#  gcc cpp
#  CPP = /usr/local/lib/gcc-lib/m68k-next-nextstep3/2.7.2/cpp

#  standard cpp
CPP = /lib/cpp

CXX = g++
CXXMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D__cplusplus -D__GNUG__ -D__GNUC__
CXXDEBUGFLAGS = 
CXXOPTIONS    =  -fhandle-exceptions -Wall -Wno-unused

CXXLINK		= $(CXX)
CXXLINKOPTIONS  = $(CXXDEBUGFLAGS) $(CXXOPTIONS)

CC           = gcc
CMAKEDEPEND  = $(TOP)/$(BINDIR)/omkdepend -D__GNUC__
CDEBUGFLAGS  = -O

CLINK        = $(CC)
CLINKOPTIONS = $(CDEBUGFLAGS) $(COPTIONS)
INSTALL = install -c

IMPORT_CPPFLAGS += -D__m68k__ -D__nextstep__ -D__OSVERSION__=3


#
# CORBA stuff
#

#omniORB2GatekeeperImplementation = OMNIORB2_TCPWRAPGK
#omniORB2GatekeeperImplementation = NO_IMPL
omniORB2GatekeeperImplementation = OMNIORB2_DUMMYGK
CorbaImplementation = OMNIORB2

#
# OMNI thread stuff
#

ThreadSystem = Mach
OMNITHREAD_CPPFLAGS = -D_REENTRANT
OMNITHREAD_LIB = $(patsubst %,$(LibSearchPattern),omnithread)

lib_depend := $(patsubst %,$(LibPattern),omnithread)
OMNITHREAD_LIB_DEPEND := $(GENERATE_LIB_DEPEND)


# Default location of the omniORB2 configuration file [falls back to this if
# the environment variable OMNIORB_CONFIG is not set] :

OMNIORB_CONFIG_DEFAULT_LOCATION = \"/etc/omniORB.cfg\"

# Default directory for the omniNames log files.
OMNINAMES_LOG_DEFAULT_LOCATION = \"/var/omninames\"
