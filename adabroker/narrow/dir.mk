
CXXSRCS       = \
		nr_narrow.cc

OBJS   = \
		nr_narrow.o

DIR_CPPFLAGS = -I../include $(patsubst %,-I%/../include,$(VPATH))

lib = $(patsubst %,$(LibPattern),narrow)

all:: $(lib)

$(lib): $(OBJS)
	@$(StaticLinkLibrary)
