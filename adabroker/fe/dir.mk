
CXXSRCS       = \
		fe_declarator.cc \
		fe_private.cc \
		fe_init.cc \
		fe_extern.cc \
		fe_interface_header.cc


OBJS	      = \
                y.tab.o \
                lex.yy.o \
		fe_declarator.o \
		fe_private.o \
		fe_init.o \
		fe_extern.o \
		fe_interface_header.o

DIR_CPPFLAGS = -I. -I../include $(patsubst %,-I%/../include,$(VPATH))

lib = $(patsubst %,$(LibPattern),fe)

all:: $(lib)

$(lib): $(OBJS)
	@$(StaticLinkLibrary)

#
# Create the parser and lexer
#
# If ever we need to regenerate lex.yy.cc and y.tab.cc, use bison and flex
#
#lex.yy.cc: idl.ll
#	@-$(RM) $@
#	flex -B $<
#	sed -e 's/^#include <unistd.h>//' lex.yy.c > $@
#	echo '#ifdef __VMS' >> $@
#	echo '// Some versions of DEC C++ for OpenVMS set the module name used by the' >> $@
#	echo '// librarian based on the last #line encountered.' >> $@
#	echo '#line' `cat $@ | wc -l` '"lex_yy.cc"' >> $@
#	echo '#endif' >> $@
#
#y.tab.cc: idl.yy
#	@-$(RM) $@
#	bison -d -y $<
#	mv -f y.tab.c y.tab.cc
#	echo '#ifdef __VMS' >> $@
#	echo '// Some versions of DEC C++ for OpenVMS set the module name used by the' >> $@
#	echo '// librarian based on the last #line encountered.' >> $@
#	echo '#line' `cat $@ | wc -l` '"y_tab.cc"' >> $@
#	echo '#endif' >> $@
#
#clean::
#	$(RM) y.tab.cc y.tab.hh y.tab.c y.tab.h lex.yy.c lex.yy.cc
