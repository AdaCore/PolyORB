#
# Standard make variables and rules for all ATMos platforms.
#

ATMos = 1
ArmProcessor = 1

#
# Any recursively-expanded variable set here can be overridden _afterwards_ by
# a platform-specific mk file which includes this one.
#

ATMOS_RELEASE_DIR = /project/atmos/release$(ATMOS_VERSION)/$(ATMOS_HARDWARE)

ATMOS_INCLUDES += pthreads llibc llibc++ timer uart atm console isfs colours ip


#
# Standard programs
#

CC                = arm-gcc
CDEBUGFLAGS       = -O3
COPTIONS          = -nostdinc -fno-common -fno-builtin -Wall -Wno-format -Wwrite-strings -Wpointer-arith -Wtraditional -Wshadow -Wstrict-prototypes -Wmissing-prototypes -Wnested-externs -Wno-parentheses
CMAKEDEPEND       = omkdepend -D__GNUC__

CXX               = arm-gcc
CXXDEBUGFLAGS     = 
CXXOPTIONS        = -D__cplusplus -nostdinc -nostdinc++ -fno-common -fno-builtin -Wall -Wno-format -Wwrite-strings -Wpointer-arith -Wtraditional -Wstrict-prototypes -Wmissing-prototypes -Wnested-externs -Wno-parentheses -Wno-unused -fhandle-exceptions
CXXMAKEDEPEND     = omkdepend -D__cplusplus -D__GNUG__ -D__GNUC__

CATOBJ            = catobj

AS                = arm-asm
ASFLAGS           = $(CPPFLAGS)
ASMAKEDEPEND      = arm-cpp -M

CP                = cp
MV		  = mv -f
MKDIRHIER         = omkdirhier

INSTALL           = installbsd -c
INSTLIBFLAGS      = -m 0644
INSTEXEFLAGS      = -m 0755


#
# Replacements for implicit rules
#

%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

%.o: %.cc
	$(CXX) -c $(CXXFLAGS) -o $@ $<

%.o: %.s
	$(AS) $(ASFLAGS) -o $@ $<


#
# Standard ATMos CPP flags.
#

IMPORT_CPPFLAGS += -D__atmos__ -D__arm__ \
		   -D__OSVERSION__=$(ATMOS_MAJOR_VERSION) \
	           $(patsubst %,-I%/$(BINDIR)/init,$(IMPORT_TREES)) \
		   $(patsubst %,-I$(ATMOS_RELEASE_DIR)/%,$(ATMOS_INCLUDES))


#
# General rule for cleaning.
#

define CleanRule
$(RM) *.o
endef

define VeryCleanRule
$(RM) *.d
$(RM) $(CORBA_STUB_FILES)
endef


#
# Patterns for various file types
#

LibPattern = %_lib.o
LibSearchPattern = %_lib.o
BinPattern = %_exe.o


#
# Stuff to generate statically-linked libraries.  Since all ATMos code needs
# to be joined into a single image this is in fact just a .o file generated
# with catobj.
#

define StaticLinkLibrary
(set -x; \
 $(RM) $@; \
 $(CATOBJ) -o $@ $^; \
)
endef

ifdef EXPORT_TREE
define ExportLibrary
(dir="$(EXPORT_TREE)/$(LIBDIR)"; \
 files="$^"; \
 for file in $$files; do \
   $(ExportFileToDir); \
 done; \
)
endef
endif


#
# Stuff to generate "executable binaries".  Since all ATMos code needs to be
# joined into a single image this is in fact just a .o file generated with
# catobj and a module file describing it (with Object/Executable directives).
#

IMPORT_LIBRARY_DIRS = $(patsubst %,%/$(LIBDIR),$(IMPORT_TREES))

define CExecutable
((set -x; $(RM) $@); \
 dirs="$(IMPORT_LIBRARY_DIRS)"; \
 fullibs=""; \
 for file in $$libs; do \
   $(FindFileInDirs); \
   duplicate=""; \
   for lib in $$fulllibs; do \
     if [ "$$fullfile" = "$$lib" ]; then \
       duplicate="true"; \
       break; \
     fi; \
   done; \
   if [ "$$duplicate" = "" ]; then \
     fulllibs="$$fulllibs $$fullfile"; \
   fi; \
 done; \
 (set -x; $(CATOBJ) -o $@ $(filter-out $(LibPattern),$^) $$fulllibs); \
)
endef

CXXExecutable=$(CExecutable)


ifdef EXPORT_TREE
define ExportExecutable
(if [ "$$module" = "" ]; then \
   module=$(patsubst $(BinPattern),%,$<); \
 fi; \
 dir="$(EXPORT_TREE)/$(BINDIR)/$$module"; \
 file="$$module.module"; \
 $(ExportFileToDir); \
 files="$^"; \
 for file in $$files; do \
   $(ExportFileToDir); \
 done; \
)
endef
endif


#
# Stuff for exporting ATMos modules and packages.
#

ifdef EXPORT_TREE
define ExportATMosInterfaces
(dir=$(EXPORT_TREE)/$(BINDIR)/init; $(CreateDir); \
 allinterfaces="$(EXPORT_TREE)/$(BINDIR)/all_interfaces"; \
 if [ -f $$allinterfaces ]; then \
   (set -x; $(RM) $$allinterfaces); \
 fi; \
 for module in $$modules; do \
   echo echo "Module $$module >> $$allinterfaces"; \
   echo "Module $$module" >> $$allinterfaces; \
   file="$$module.module"; \
   dir="$(EXPORT_TREE)/$(BINDIR)/$$module"; \
   $(ExportFileToDir); \
 done; \
 for hardware in $$hardwares; do \
   echo echo "Hardware $$hardware >> $$allinterfaces"; \
   echo "Hardware $$hardware" >> $$allinterfaces; \
   file="$$hardware.hw"; \
   dir="$(EXPORT_TREE)/$(BINDIR)/hardware"; \
   $(ExportFileToDir); \
 done; \
 systemfile="$(EXPORT_TREE)/$(BINDIR)/SYSTEM"; \
 if [ -f $$systemfile ]; then \
   (set -x; $(RM) $$systemfile); \
 fi; \
 echo echo "Hardware $(ATMOS_HARDWARE) > $$systemfile"; \
 echo "Hardware $(ATMOS_HARDWARE)" > $$systemfile; \
 echo echo "Set Pthreads >> $$systemfile"; \
 echo "Set Pthreads" >> $$systemfile; \
 echo echo "Package core >> $$systemfile"; \
 echo "Package core" >> $$systemfile; \
 importtrees="$(IMPORT_TREES)"; \
 revimporttrees=""; \
 for importtree in $$importtrees; do \
   if [ -f "$$importtree/$(BINDIR)/all_interfaces" ]; then \
     echo echo "Path $$importtree/$(BINDIR) >> $$systemfile"; \
     echo "Path $$importtree/$(BINDIR)" >> $$systemfile; \
     revimporttrees="$$importtree $$revimporttrees"; \
   fi; \
 done; \
 for importtree in $$revimporttrees; do \
   echo "cat $$importtree/$(BINDIR)/all_interfaces >> $$systemfile"; \
   cat $$importtree/$(BINDIR)/all_interfaces >> $$systemfile; \
 done; \
 (set -x; cd $(EXPORT_TREE)/$(BINDIR); aconfig); \
)
endef

define ExportATMosPackages
(for package in $$packages; do \
   file="$$package.pkg"; \
   dir="$(EXPORT_TREE)/$(BINDIR)/software"; \
   $(ExportFileToDir); \
 done; \
)
endef
else
ExportATMosInterfaces=$(NoExportTreeError)
ExportATMosPackages=$(NoExportTreeError)
endif



#
# OMNI thread stuff
#

ThreadSystem = Posix
OMNITHREAD_POSIX_CPPFLAGS = -DPthreadDraftVersion=6 \
			    -DPthreadSupportThreadPriority -DNeedPthreadInit
OMNITHREAD_CPPFLAGS =
OMNITHREAD_LIB = $(patsubst %,$(LibSearchPattern),omnithread)
lib_depend := $(patsubst %,$(LibPattern),omnithread)
OMNITHREAD_LIB_DEPEND := $(GENERATE_LIB_DEPEND)



#
# CORBA stuff
#

CorbaImplementation = OMNIORB2

OMNIORB2_IDL_ONLY = omniidl2
OMNIORB2_IDL_ANY_FLAGS = -a
OMNIORB2_IDL = $(OMNIORB2_IDL_ONLY) $(OMNIORB2_IDL_ANY_FLAGS)
OMNIORB2_CPPFLAGS = -D__OMNIORB2__ -I$(CORBA_STUB_DIR) $(OMNITHREAD_CPPFLAGS)
OMNIORB2_LIB = $(patsubst %,$(LibSearchPattern),omniORB2) $(OMNITHREAD_LIB) $(SOCKET_LIB)
lib_depend := $(patsubst %,$(LibPattern),omniORB2)
OMNIORB2_LIB_DEPEND := $(GENERATE_LIB_DEPEND) $(OMNITHREAD_LIB_DEPEND)
OMNIORB2_STUB_HDR_PATTERN = $(CORBA_STUB_DIR)/%.hh
OMNIORB2_STUB_SRC_PATTERN = $(CORBA_STUB_DIR)/%SK.cc
OMNIORB2_STUB_OBJ_PATTERN = $(CORBA_STUB_DIR)/%SK.o

# omniORB2 access control policy modules

OMNIORB2_DUMMYGK_LIB = $(patsubst %,$(LibSearchPattern),omniGK_stub)
lib_depend := $(patsubst %,$(LibPattern),omniGK_stub)
OMNIORB2_DUMMYGK_LIB_DEPEND := $(GENERATE_LIB_DEPEND)

OMNIORB2_TCPWRAPGK_LIB = $(patsubst %,$(LibSearchPattern),tcpwrapGK)
lib_depend := $(patsubst %,$(LibPattern),tcpwrapGK)
OMNIORB2_TCPWRAPGK_LIB_DEPEND := $(GENERATE_LIB_DEPEND)

omniORB2GatekeeperImplementation = OMNIORB2_DUMMYGK

OMNIORB2_LIB += $($(omniORB2GatekeeperImplementation)_LIB)
OMNIORB2_LIB_DEPEND += $($(omniORB2GatekeeperImplementation)_LIB_DEPEND)

# LifeCycle stuff

OMNIORB2_IDL_LC_FLAGS = -l
OMNIORB2_LC_LIB = $(patsubst %,$(LibSearchPattern),omniLC)
