#
# x86_osr5.mk - make variables and rules specific to SCO OpenServer 5
#
# This file is created by Rod Record <rr@sco.com>. He wrote:
#   I have recently built omniORB 2.5.0 on SCO OpenServer 5. I'm in the
#   process of testing and refining this build. In order to successfully
#   build on SCO OpenServer, it was necessary to apply the patches below
#   and utilize the x86_osr5.mk configuration file (also below).
#
#   I built on SCO OpenServer 5.0.4 with the GNU development system and
#   FSU Pthreads library from SCO Skunkware :
#     http://www.sco.com/skunkware/osr5/devtools/gcc/
#     http://www.sco.com/skunkware/osr5/libraries/pthreads/
#

OSR5 = 1
x86Processor = 1


#
# Include general unix things
#

include $(THIS_IMPORT_TREE)/mk/unix.mk


#
# C preprocessor macro definitions for this architecture
#

IMPORT_CPPFLAGS += -D__x86__ -D__osr5__ -D__OSVERSION__=5


#
# Standard programs
#

AR = ar cq
RANLIB = true
INSTALL = install -c
MKDIRHIER = mkdirhier


# Use gcc from SCO Skunkware:
#    http://www.sco.com/skunkware/osr5/devtools/gcc/ 
#
CPP = gcc
#
CXX = g++
CXXMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D__cplusplus -D__GNUG__ -D__GNUC__
CXXDEBUGFLAGS = 
CXXOPTIONS    =  -fhandle-exceptions -Wall -Wno-unused
CXXMTFLAG     =
#
CXXLINK		= $(CXX)
CXXLINKOPTIONS  = $(CXXDEBUGFLAGS) $(CXXOPTIONS)

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
# Use FSU Pthread library from SCO Skunkware:
#     http://www.sco.com/skunkware/osr5/libraries/pthreads/
#
OpenServer_OMNITHREAD_LIB = $(patsubst %,$(LibSearchPattern),omnithread) \
			 -lgthreads $(CXXMTFLAG)
OpenServer_OMNITHREAD_CPPFLAGS = $(CXXMTFLAG)

Posix_OMNITHREAD_LIB = $(patsubst %,$(LibSearchPattern),omnithread) -lgthreads \
		 $(CXXMTFLAG)
Posix_OMNITHREAD_CPPFLAGS = -DUsePthread $(CXXMTFLAG)
OMNITHREAD_POSIX_CPPFLAGS = -DPthreadDraftVersion=6 \
			    -DPthreadSupportThreadPriority -DNoNanoSleep


OMNITHREAD_LIB = $($(ThreadSystem)_OMNITHREAD_LIB)
OMNITHREAD_CPPFLAGS = $($(ThreadSystem)_OMNITHREAD_CPPFLAGS)

ThreadSystem = Posix

lib_depend := $(patsubst %,$(LibPattern),omnithread)
OMNITHREAD_LIB_DEPEND := $(GENERATE_LIB_DEPEND)


#
# OMNI ParTcl stuff
#

TCLTK_CPPFLAGS = -I/usr/local/include
TCLTK_LIB = -L/usr/local/lib -ltk8.0 -ltcl8.0 -lm
X11_CPPFLAGS = 
X11_LIB = -lX11
WISH4 = /usr/local/bin/wish

OMNIPARTCL_CPPFLAGS = $(TCLTK_CPPFLAGS) $(X11_CPPFLAGS) $(OMNITHREAD_CPPFLAGS)
OMNIPARTCL_LIB = $(patsubst %,$(LibSearchPattern),omniParTcl) $(TCLTK_LIB) \
		 $(X11_LIB) $(OMNITHREAD_LIB)
lib_depend := $(patsubst %,$(LibPattern),omniParTcl)
OMNIPARTCL_LIB_DEPEND := $(GENERATE_LIB_DEPEND) $(OMNITHREAD_LIB_DEPEND)

#
# Java and Java/CORBA stuff
#

JAVA_ROOT = /usr/java
JAVAIDL_ROOT = /usr/local/javaIDL

# Default location of the omniORB2 configuration file [falls back to this if
# the environment variable OMNIORB_CONFIG is not set] :

OMNIORB_CONFIG_DEFAULT_LOCATION = \"/usr/local/etc/omniORB.cfg\"

# Default directory for the omniNames log files.
OMNINAMES_LOG_DEFAULT_LOCATION = \"/usr/local/var/omninames\"
