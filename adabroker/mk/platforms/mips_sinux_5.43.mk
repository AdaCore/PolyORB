#
# mips_sinux_5.43.mk - make variables and rules specific to Reliant UNIX 5.43
#

SINIX = 1

#
# Include general unix things
#

include $(THIS_IMPORT_TREE)/mk/unix.mk


#
# C preprocessor macro definitions for this architecture
#

IMPORT_CPPFLAGS += -D__SINIX__ -DYYDEBUG


#
# Standard programs
#

AR = ar cq


#
# To use CDS++
#
CPP = CC

CXX = CC
CXXMAKEDEPEND = $(TOP)/$(BINDIR)/omkdepend -D__cplusplus
CXXDEBUGFLAGS = -g

CXXLINK		= $(CXX)
CXXOPTIONS  = -Kthread -KPIC
CXXLINKOPTIONS  = $(CXXDEBUGFLAGS) $(CXXOPTIONS) \
		  $($(IMPORT_LIBRARY_DIRS))


CC           = cc
CMAKEDEPEND  = $(TOP)/$(BINDIR)/omkdepend -D__GNUC__
CDEBUGFLAGS  = -g

CLINK        = $(CC)
COPTIONS  = -KPIC
CLINKOPTIONS = $(CDEBUGFLAGS) $(COPTIONS) \
		  $($(IMPORT_LIBRARY_DIRS))

INSTALL = /usr/ucb/install -c


#
# Socket library
#

SOCKET_LIB = -lsocket -lnsl
THREAD_LIB = 


#
# CORBA stuff
#

CorbaImplementation = OMNIORB2

#
# OMNI thread stuff
#

ThreadSystem = Posix
OMNITHREAD_POSIX_CPPFLAGS = -Kthread
OMNITHREAD_CPPFLAGS = -D_REENTRANT -DUsePthread
OMNITHREAD_LIB = $(patsubst %,$(LibSearchPattern),omnithread)

OMNITHREAD_POSIX_CPPFLAGS += -DPthreadDraftVersion=4 -DNoNanoSleep
#OMNITHREAD_LIB += -lpthread


lib_depend := $(patsubst %,$(LibPattern),omnithread)
OMNITHREAD_LIB_DEPEND := $(GENERATE_LIB_DEPEND)

#
# OMNI ParTcl stuff
#

TCLTK_CPPFLAGS = -I/usr/local/include/Tcl7.4Tk4.0
TCLTK_LIB = -L/usr/local/lib -ltk4.0 -ltcl7.4 -lm -R /usr/local/lib
X11_CPPFLAGS = -I/usr/openwin/include
X11_LIB = -L/usr/openwin/lib -lX11 -R /usr/openwin/lib
WISH4 = /usr/local/bin/wish4.0

OMNIPARTCL_CPPFLAGS = $(TCLTK_CPPFLAGS) $(X11_CPPFLAGS) $(OMNITHREAD_CPPFLAGS)
OMNIPARTCL_LIB = $(patsubst %,$(LibSearchPattern),omniParTcl) $(TCLTK_LIB) \
		 $(X11_LIB) $(OMNITHREAD_LIB)
lib_depend := $(patsubst %,$(LibPattern),omniParTcl)
OMNIPARTCL_LIB_DEPEND := $(GENERATE_LIB_DEPEND) $(OMNITHREAD_LIB_DEPEND)


clean ::
	rm -f *.o.ii
