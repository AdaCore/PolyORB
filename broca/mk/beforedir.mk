#############################################################################
#
# MakeSubdirs is a general rule which runs omake in each of SUBDIRS.
# Unfortunately we have to unset MAKEFLAGS otherwise the -I flags which
# omake passed to this make will be incorrectly passed down to the sub-make.
#

define MakeSubdirs
(unset MAKEFLAGS; \
 set -e; \
 if [ "$$subdir_makeflags" = "" ]; then \
   subdir_makeflags='$(SUBDIR_MAKEFLAGS)'; \
 fi; \
 if [ "$$subdirs" = "" ]; then \
   subdirs='$(SUBDIRS)'; \
 fi; \
 if [ "$$target" = "" ]; then \
   target='$@'; \
 fi; \
 for dir in $$subdirs ; do \
   $(CreateDir); \
   (cd $$dir ; echo "making $$target in $(CURRENT)/$$dir..." ; \
    eval $(MAKE) $$subdir_makeflags $$target ) ; \
   if [ $$? != 0 ]; then \
     exit 1; \
   fi; \
 done; \
)
endef

# Stop SUBDIRS specified on the command line being passed down.
unexport SUBDIRS


#############################################################################
#
# Useful bits of shell script.  Most take arguments as shell variables which
# can be set before putting them in your make rule.
#

#
# Create directory $$dir if it doesn't already exist.
#

define CreateDir
if [ ! -d $$dir ]; then \
   (umask 002; set -x; $(MKDIRHIER) $$dir); \
fi
endef


#
# Find $$file in $$dirs.  Returns full file name in $$fullfile.
#

define FindFileInDirs
case "$$file" in \
/*) fullfile="$$file";; \
*) \
  fullfile=""; \
  for _dir in $$dirs; do \
    if [ -f $$_dir/$$file ]; then \
      if [ "$$_dir" = "." ]; then \
        fullfile="$$file"; \
      else \
        fullfile="$$_dir/$$file"; \
      fi; \
      break; \
    fi; \
  done; \
  if [ ! "$$fullfile" ]; then \
    echo "ERROR: Cannot find $$file in $$dirs"; \
    exit 1; \
  fi;; \
esac
endef


#
# Find $$file in current directory or $(VPATH) - returns $$fullfile.
#

define FindFileInVpath
dirs='. $(VPATH)'; \
$(FindFileInDirs)
endef


#
# "Export" $$file to $$dir, creating $$dir if necessary.  Searches for
# $$file in $(VPATH) if not found in current directory.
#

define ExportFileToDir
$(CreateDir); \
$(FindFileInVpath); \
base=`basename $$file`; \
if [ -f $$dir/$$base ] && cmp $$fullfile $$dir/$$base >/dev/null; then \
  echo "File $$base hasn't changed."; \
else \
  (set -x; \
   $(INSTALL) $(INSTLIBFLAGS) $$fullfile $$dir); \
fi
endef


#
# "Export" an executable file.  Same as previous one but adds execute
# permission.
#

define ExportExecutableFileToDir
$(CreateDir); \
$(FindFileInVpath); \
base=`basename $$file`; \
if [ -f $$dir/$$base ] && cmp $$fullfile $$dir/$$base >/dev/null; then \
  echo "File $$base hasn't changed."; \
else \
  (set -x; \
   $(INSTALL) $(INSTEXEFLAGS) $$fullfile $$dir); \
fi
endef


#############################################################################
#
# CORBA stuff
#

vpath %.idl $(IMPORT_TREES:%=%/idl)

CORBA_IDL_FILES = $(CORBA_INTERFACES:%=%.idl)

CORBA_STUB_DIR = $(TOP)/stub
CORBA_STUB_DIR_TO_TOP = ..

CorbaImplementation = NO_CORBA_IMPLEMENTATION

CORBA_IDL		= $($(CorbaImplementation)_IDL)
CORBA_CPPFLAGS		= $($(CorbaImplementation)_CPPFLAGS)
CORBA_LIB		= $($(CorbaImplementation)_LIB)
CORBA_LIB_DEPEND	= $($(CorbaImplementation)_LIB_DEPEND)
CORBA_LIB_NODYN		= $($(CorbaImplementation)_LIB_NODYN)
CORBA_LIB_NODYN_DEPEND	= $($(CorbaImplementation)_LIB_NODYN_DEPEND)

CORBA_STUB_HDR_PATTERN	= $($(CorbaImplementation)_STUB_HDR_PATTERN)
CORBA_STUB_SRC_PATTERN	= $($(CorbaImplementation)_STUB_SRC_PATTERN)
CORBA_STUB_OBJ_PATTERN	= $($(CorbaImplementation)_STUB_OBJ_PATTERN)
CORBA_DYN_STUB_SRC_PATTERN = $($(CorbaImplementation)_DYN_STUB_SRC_PATTERN)
CORBA_DYN_STUB_OBJ_PATTERN = $($(CorbaImplementation)_DYN_STUB_OBJ_PATTERN)

CORBA_STUB_HDRS		= $(CORBA_INTERFACES:%=$(CORBA_STUB_HDR_PATTERN))
CORBA_STUB_SRCS		= $($(CorbaImplementation)_STUB_SRCS)
CORBA_STUB_OBJS		= $($(CorbaImplementation)_STUB_OBJS)
CORBA_STATIC_STUB_SRCS	= $($(CorbaImplementation)_STATIC_STUB_SRCS)
CORBA_STATIC_STUB_OBJS	= $($(CorbaImplementation)_STATIC_STUB_OBJS)
CORBA_DYN_STUB_SRCS	= $($(CorbaImplementation)_DYN_STUB_SRCS)
CORBA_DYN_STUB_OBJS	= $($(CorbaImplementation)_DYN_STUB_OBJS)

CORBA_STUB_FILES = $(CORBA_INTERFACES:%=$(CORBA_STUB_DIR)/%.idl) \
		   $(CORBA_STUB_HDRS) $(CORBA_STUB_SRCS) $(CORBA_STUB_OBJS) \
		   $(CORBA_STUB_OBJS:.o=.d) $(CORBA_STUB_DIR)/dir.mk \
		   $($(CorbaImplementation)_EXTRA_STUB_FILES)

GENERATED_CXX_HDRS += $(CORBA_STUB_HDRS)

