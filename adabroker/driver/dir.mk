
CXXSRCS       = \
		drv_init.cc \
		drv_private.cc \
		drv_main.cc \
		drv_args.cc \
		drv_fork.cc \
		drv_link.cc \
		drv_preproc.cc

OBJS = \
		drv_init.o \
		drv_private.o \
		drv_main.o \
		drv_args.o \
		drv_fork.o \
		drv_link.o \
		drv_preproc.o

# IDL_CFE_VERSION       Defines what the current IDL CFE version is
IDL_CFE_VERSION =       1.3.0

ifdef OSF1

# The front end assumes that line information is produced by cpp before
# the first line of an included file in this form:
#    # 1 "file name"
# Cpp on DEC AXP OSF1 v2 produce line information like this:
#    # 2 "file name"
# after the first line instead of before. This cause the front end 
# to misinterpret the first line as one that comes from the including file.
# Therefore we have to use gcc as the preprocessor instead.

CPP_LOCATION = gcc

else

ifdef Linux

CPP_LOCATION = gcc

else

CPP_LOCATION = $(CPP)

endif

endif


ifdef Win32Platform
# Note the use of -W0 in CXXOPTIONS - this is used to stop pragma warning 
# messages - should replace with a warning(disable,4068) pragma in the source
# code
CPP_LOCATION = CL

DIR_CPPFLAGS = -I../include $(patsubst %,-I%/../include,$(VPATH)) \
               -DIDL_CFE_VERSION='"$(IDL_CFE_VERSION)"' \
               -DCPP_LOCATION=\"$(CPP_LOCATION)\"

else
DIR_CPPFLAGS = -I../include $(patsubst %,-I%/../include,$(VPATH)) \
               -DIDL_CFE_VERSION='"$(IDL_CFE_VERSION)"' \
               -DCPP_LOCATION=\"$(CPP_LOCATION)\"

ifdef SunOS
DIR_CPPFLAGS += -DSVR4
endif

endif

lib = $(patsubst %,$(LibPattern),drv)

ifdef Win32Platform

all:: $(OBJS)

else

all:: $(lib)
$(lib): $(OBJS)
	@$(StaticLinkLibrary)

endif
