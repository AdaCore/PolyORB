
CXXSRCS       = \
		utl_scope.cc \
		utl_stack.cc \
		utl_string.cc \
		utl_decllist.cc \
		utl_labellist.cc \
		utl_namelist.cc \
		utl_exceptlist.cc \
		utl_strlist.cc \
		utl_list.cc \
		utl_exprlist.cc \
		utl_error.cc \
		utl_indenter.cc \
		utl_identifier.cc \
		utl_idlist.cc \
		utl_global.cc

OBJS     = \
		utl_scope.o \
		utl_stack.o \
		utl_string.o \
		utl_decllist.o \
		utl_labellist.o \
		utl_namelist.o \
		utl_exceptlist.o \
		utl_strlist.o \
		utl_list.o \
		utl_exprlist.o \
		utl_error.o \
		utl_indenter.o \
		utl_identifier.o \
		utl_idlist.o \
		utl_global.o

DIR_CPPFLAGS = -I../include $(patsubst %,-I%/../include,$(VPATH))

lib = $(patsubst %,$(LibPattern),util)

all:: $(lib)

$(lib): $(OBJS)
	@$(StaticLinkLibrary)
