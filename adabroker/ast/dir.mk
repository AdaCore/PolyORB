
CXXSRCS	      = \
		ast_array.cc \
		ast_argument.cc \
		ast_attribute.cc \
		ast_check.cc \
		ast_concrete_type.cc \
		ast_constant.cc \
		ast_decl.cc \
		ast_enum.cc \
		ast_enum_val.cc \
		ast_exception.cc \
		ast_expression.cc \
		ast_field.cc \
		ast_interface.cc \
		ast_intf_fwd.cc \
		ast_module.cc \
		ast_operation.cc \
		ast_predefined_type.cc \
		ast_root.cc \
		ast_sequence.cc \
		ast_string.cc \
		ast_structure.cc \
		ast_type.cc \
		ast_typedef.cc \
		ast_union.cc \
		ast_union_branch.cc \
		ast_union_label.cc \
		ast_generator.cc \
		ast_redef.cc \
		ast_recursive.cc


OBJS	= \
		ast_array.o \
		ast_argument.o \
		ast_attribute.o \
		ast_check.o \
		ast_concrete_type.o \
		ast_constant.o \
		ast_decl.o \
		ast_enum.o \
		ast_enum_val.o \
		ast_exception.o \
		ast_expression.o \
		ast_field.o \
		ast_interface.o \
		ast_intf_fwd.o \
		ast_module.o \
		ast_operation.o \
		ast_predefined_type.o \
		ast_root.o \
		ast_sequence.o \
		ast_string.o \
		ast_structure.o \
		ast_type.o \
		ast_typedef.o \
		ast_union.o \
		ast_union_branch.o \
		ast_union_label.o \
		ast_generator.o \
		ast_redef.o \
		ast_recursive.o

DIR_CPPFLAGS = -I../include $(patsubst %,-I%/../include,$(VPATH))

lib = $(patsubst %,$(LibPattern),ast)

all:: $(lib)

$(lib): $(OBJS)
	@$(StaticLinkLibrary)
