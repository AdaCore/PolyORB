CXXSRCS       = \
		o2be_array.cc \
		o2be_attribute.cc \
		o2be_constant.cc \
		o2be_enum.cc \
		o2be_exception.cc \
		o2be_field.cc \
		o2be_generator.cc \
		o2be_interface.cc \
		o2be_module.cc \
		o2be_name.cc \
		o2be_operation.cc \
		o2be_predefined_type.cc \
		o2be_root.cc \
		o2be_sequence.cc \
		o2be_string.cc \
		o2be_struct.cc \
		o2be_typedef.cc \
		o2be_union.cc \
		o2be_union_branch.cc \
		o2be_buildDesc.cc \
		o2be_name_mangle.cc \
		o2be_call_desc.cc

OBJS         = \
		o2be_array.o \
		o2be_attribute.o \
		o2be_constant.o \
		o2be_enum.o \
		o2be_exception.o \
		o2be_field.o \
		o2be_generator.o \
		o2be_interface.o \
		o2be_module.o \
		o2be_name.o \
		o2be_operation.o \
		o2be_predefined_type.o \
		o2be_root.o \
		o2be_sequence.o \
		o2be_string.o \
		o2be_struct.o \
		o2be_typedef.o \
		o2be_union.o \
		o2be_union_branch.o \
		o2be_buildDesc.o \
		o2be_name_mangle.o \
		o2be_call_desc.o

DIR_CPPFLAGS = -I. -I../include $(patsubst %,-I%/../include,$(VPATH))

ifdef Win32Platform
CXXOPTIONS += -Zm200
endif

lib = $(patsubst %,$(LibPattern),omniORB2_be)


all:: $(lib)


$(lib): $(OBJS)
	@$(StaticLinkLibrary)
