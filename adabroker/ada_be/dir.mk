CXXSRCS       = \
		adabe_struct.cc\
		adabe_name.cc\
		adabe_module.cc\
		adabe_array.cc\
		adabe_enum.cc\
		adabe_enum_val.cc\
		adabe_exception.cc\
		adabe_string.cc\
		adabe_constant.cc\
		adabe_string_list.cc\
		adabe_root.cc\
		adabe_UnionBranch.cc\
		adabe_union.cc\
		adabe_interface.cc\
		adabe_attribute.cc\
		adabe_operation.cc\
		adabe_typedef.cc\
		adabe_argument.cc\
		adabe_field.cc\
		adabe_predefined_type.cc\
		adabe_interface_fwd.cc\
		adabe_generator.cc


OBJS         = \
		adabe_struct.o\
		adabe_module.o\
		adabe_name.o\
		adabe_array.o\
		adabe_enum.o\
		adabe_enum_val.o\
		adabe_string.o\
		adabe_constant.o\
		adabe_string_list.o\
		adabe_root.o\
		adabe_UnionBranch.o\
		adabe_union.o\
		adabe_interface.o\
		adabe_attribute.o\
		adabe_operation.o\
		adabe_typedef.o\
		adabe_argument.o\
		adabe_field.o\
		adabe_predefined_type.o\
		adabe_interface_fwd.o\
		adabe_generator.o

DIR_CPPFLAGS = -I. -I../include $(patsubst %,-I%/../include,$(VPATH)) -I../omniORB2_be

ifdef Win32Platform
CXXOPTIONS += -Zm200
endif

lib = $(patsubst %,$(LibPattern),ada_be)


all:: $(lib)


$(lib): $(OBJS)
	@$(StaticLinkLibrary)
