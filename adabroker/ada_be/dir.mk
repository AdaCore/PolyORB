CXXSRCS       = \
		adabe_root.cc\
		adabe_name.cc\
		adabe_module.cc\
		adabe_array.cc\
		adabe_enum.cc\
		adabe_enum_val.cc\
		adabe_exception.cc\
		adabe_string.cc\
		adabe_constant.cc\
		adabe_string_list.cc


OBJS         = \
		adabe_root.o\
		adabe_module.o\
		adabe_name.o\
		adabe_array.o\
		adabe_enum.o\
		adabe_enum_val.o\
		adabe_string.o\
		adabe_constant.o\
		adabe_string_list.o

DIR_CPPFLAGS = -I. -I../include $(patsubst %,-I%/../include,$(VPATH)) -I../omniORB2_be

ifdef Win32Platform
CXXOPTIONS += -Zm200
endif

lib = $(patsubst %,$(LibPattern),ada_be)


all:: $(lib)


$(lib): $(OBJS)
	@$(StaticLinkLibrary)
