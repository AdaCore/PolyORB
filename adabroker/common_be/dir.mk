CXXSRCS       = \
		cfe_interface.cc

OBJS         = \
		cfe_interface.o

DIR_CPPFLAGS = -I. -I../include $(patsubst %,-I%/../include,$(VPATH)) -I../omniORB2_be -I../ada_be

ifdef Win32Platform
CXXOPTIONS += -Zm200
endif

lib = $(patsubst %,$(LibPattern),common_be)


all:: $(lib)


$(lib): $(OBJS)
	@$(StaticLinkLibrary)
