SUBDIRS = common_be ast driver fe narrow util ada_be

ifdef Win32Platform
DRV_OBJS =      driver/drv_init.o \
                driver/drv_private.o \
                driver/drv_main.o \
                driver/drv_args.o \
                driver/drv_fork.o \
                driver/drv_link.o \
                driver/drv_preproc.o

LIBS = advapi32.lib

ifndef BuildDebugBinary
CXXDEBUGFLAGS =
SUBDIR_MAKEFLAGS = CXXDEBUGFLAGS=""
endif

else

OBJ_LIBS = driver/$(patsubst %,$(LibPattern),drv)

endif

OBJ_LIBS += \
           ast/$(patsubst %,$(LibPattern),ast) \
	   common_be/$(patsubst %,$(LibPattern),common_be)\
           fe/$(patsubst %,$(LibPattern),fe) \
           util/$(patsubst %,$(LibPattern),util) \
           narrow/$(patsubst %,$(LibPattern),narrow)\
	   ada_be/$(patsubst %,$(LibPattern),ada_be)

LIBS += $(OBJ_LIBS)

ifdef SINIX
LIBS += -L/usr/ucblib -lucb
endif

all::
	@$(MakeSubdirs)

prog = $(patsubst %,$(BinPattern),adabroker)

all:: $(prog)

$(prog): $(DRV_OBJS) $(OBJ_LIBS)
	@(libs="$(LIBS) $(LIBS)"; $(CXXExecutable))

clean::
	rm adabroker


