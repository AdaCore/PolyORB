# $Id$

define(POLYORB_RELEASE, ifdef([OVERRIDE_RELEASE],OVERRIDE_RELEASE,[20.0w]))

AC_PREREQ(2.60)
AC_INIT(PolyORB, pkg_version(POLYORB_RELEASE), polyorb-bugs@lists.adacore.com)

AC_CONFIG_SRCDIR(src/polyorb.ads)
AC_CONFIG_AUX_DIR(support)

AM_SCM
POLYORB_VERSION="$PACKAGE_VERSION (rev. $CMREVISION)"
AC_SUBST(POLYORB_VERSION)

AC_MSG_NOTICE([Configuring PolyORB $POLYORB_VERSION])

ifdef([LONGEST_FILE_NAME],[
if test ! -f ${srcdir}/LONGEST_FILE_NAME; then
  AC_MSG_ERROR([Source directory sanity check failed, check your distribution!

Note that this package must be extracted with GNU tar.])
fi])

##########################################
# Initialization.
##########################################

AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE

# Disable Automake rules to rebuild Makefile.in and configure.
# We handle this manually using support/reconfig.

AM_MAINTAINER_MODE

LIBVERSIONINFO=1:4:0
AC_SUBST(LIBVERSIONINFO)

# Check for developer (debug) mode.
# This must come before AC_PROG_CC so we can set the default CFLAGS ourselves.
# Note that these default CFLAGS are used for both the native and the cross
# compilers.

define(DEBUG_OPTIONS, [dnl
  if test "${CFLAGS-unset}" = "unset"; then CFLAGS="-g -O0"; fi
  BASE_ADAFLAGS="-XBuild=DEBUG $VALIDITY_FLAG"
  BARGS=-E
  ASSERTION_POLICY_DEFAULT=Check
  DEBUG_POLICY_DEFAULT=Check
  DEBUG_ONLY=""
  INSTALL_BIN='${INSTALL_PROGRAM}'
  debug=true])

define(NODEBUG_OPTIONS, [dnl
  if test "${CFLAGS-unset}" = "unset"; then CFLAGS="-g -O2"; fi
  BASE_ADAFLAGS="-XBuild=PRODUCTION"
  BARGS=
  DEBUG_ONLY="--  "
  INSTALL_BIN='${]ifdef([STRIP_PRODUCTION_BINARIES],
          [INSTALL_STRIP_PROGRAM],
          [INSTALL_PROGRAM])[}'
  debug=false])

# By default use no-debug options
NODEBUG_OPTIONS

AC_MSG_CHECKING([debugging mode])
AC_ARG_ENABLE(debug,
AS_HELP_STRING([--enable-debug], [Turn on debugging options (yes/all/no)]),
[
AC_MSG_RESULT($enableval)
if test "$enableval" = "all"; then
  VALIDITY_FLAG="-gnatVa"
  enableval=yes
fi
if test "$enableval" = "yes"; then
  DEBUG_OPTIONS
fi
],
[AC_MSG_RESULT(no)])

# LIBRARY_TYPE set to use static or shared libraries consistently.

# By default build static libs
LIBRARY_TYPE=static

AC_ARG_ENABLE(shared,
AS_HELP_STRING([--enable-shared], [Enable build of shared libraries]),
[
if test "$enableval" = "yes"; then
  LIBRARY_TYPE="relocatable"
fi
],
[])
AC_SUBST(LIBRARY_TYPE)

# WARNINGS_MODE can be set to:
#  e (treat warnings as errors)
#  n (normal processing)
#  s (suppress all warnings)

# Here we set WARNINGS_MODE to "e", unless another default has been set
# in aclocal.m4 (see prepare_distrib).

WARNINGS_MODE=ifdef([DEFAULT_WARNINGS_MODE],DEFAULT_WARNINGS_MODE,e)

AC_ARG_ENABLE(warnings,
AS_HELP_STRING([--enable-warnings=X], [Set warnings processing to X (e/n/s)]),
[case "$enableval" in
   yes)     WARNINGS_MODE=e ;;
   no)      WARNINGS_MODE=s ;;
   [[ens]]) WARNINGS_MODE="$enableval" ;;
   *)       AC_ERROR([invalid argument to --enable-warnings: $enableval]) ;;
esac])
AC_SUBST(WARNINGS_MODE)

AM_ARG_ENABLE_POLICY(assertion, Ignore)
AM_ARG_ENABLE_POLICY(debug, Check)

AC_SUBST(INSTALL_BIN)

#######################################
# Additional configuration pragmas file
#######################################

AC_ARG_ENABLE(conf-pragmas,
AS_HELP_STRING([--enable-conf-pragmas=F],
               [Additional Ada configuration pragmas file for target]),
[
  if test -f "${enableval}"; then
    # Compute absolute path, as configure tests are run from a temporary
    # directory. This variable is used only for configure tests (which are
    # executed from a temporary directory).

    ADDITIONAL_CONF_PRAGMAS_SWITCH="-gnatec=$(cd "$(dirname "${enableval}")"; pwd)/$(basename "${enableval}")"

    # Set substitution for config.adc.in

    ADDITIONAL_CONF_PRAGMAS="${enableval}"
  else
    AC_ERROR([valid file name required for --enable-conf-pragmas])
  fi
],
[ADDITIONAL_CONF_PRAGMAS="/dev/null"])
AC_SUBST_FILE(ADDITIONAL_CONF_PRAGMAS)

##########################################
# Set platform-specific supplementary CFLAGS and options
##########################################

# Host

case "$host_os" in
  cygwin* | mingw*)
    HOST_EXE_SUFFIX=.exe
    WINDOWS_ON_HOST=True

    # Make sure every call to ln(1) fails, and falls back to cp(1), because
    # the compiler does not know how to handle Cygwin symbolic links.

    SYMLINK_ALIAS="alias ln=/bin/false"
    ;;

  *)
    HOST_EXE_SUFFIX=
    WINDOWS_ON_HOST=False
    ;;
esac
AC_SUBST(WINDOWS_ON_HOST)
AM_CONDITIONAL(WINDOWS, test x$WINDOWS_ON_HOST = xTrue)

# Target

WINDOWS_ON_TARGET=False
TARGET_EXE_SUFFIX=
case $target_os in
  aix*)
    CFLAGS_FOR_TARGET="-mminimal-toc"
    ;;

  vxworks*)
    CFLAGS_FOR_TARGET="-mlongcall"
    TARGET_EXE_SUFFIX=.out
    PLATFORM_BASE_LIST="PolyORB.Parameters.Static"
    ;;

  cygwin* | mingw* )
    WINDOWS_ON_TARGET=True
    TARGET_EXE_SUFFIX=.exe
    ;;
esac
ADAFLAGS_FOR_TARGET="${CFLAGS_FOR_TARGET}"
AC_SUBST(CFLAGS_FOR_TARGET)
AC_SUBST(ADAFLAGS_FOR_TARGET)
AC_SUBST(WINDOWS_ON_TARGET)

#  Implementation note: the *vxworks* rule above is for VxWorks 5, it can
#    present some problems for VxWorks 6.

AC_SUBST(HOST_EXE_SUFFIX)
AC_SUBST(TARGET_EXE_SUFFIX)

# By default treat all warnings as errors when building directly from the
# source repository. The packaging procedure changes the default to "n".
# so that builds from packaged sources do not stop on warnings.

BASE_ADAFLAGS="${BASE_ADAFLAGS} -XWarnings_Mode=${WARNINGS_MODE}"

# ADAFLAGS will be set later, once AC_PROG_CC has set the definitive CFLAGS

AC_SUBST(CFLAGS)
AC_SUBST(ADAFLAGS)

AC_SUBST(BARGS)
AC_SUBST(DEBUG_ONLY)
AM_CONDITIONAL(DEBUG, test x$debug = xtrue)

##########################################
# Check for various programs
##########################################

AC_PROG_CC
if test "${ADAFLAGS-unset}" = unset; then
  ADAFLAGS="-cargs ${CFLAGS} -margs ${BASE_ADAFLAGS}"
else
  ADAFLAGS="-cargs ${ADAFLAGS} -margs ${BASE_ADAFLAGS}"
fi

AC_PROG_AWK
AC_CHECK_PROG(RM, rm, rm)
AC_CHECK_PROG(SED, sed, sed)
AC_CHECK_PROG(FIND, find, find)
AC_CHECK_PROG(GREP, grep, grep)
AM_PATH_PYTHON
AM_PROG_GNATCHOP
AM_PROG_ADA
AM_CROSS_PROG_ADA
AM_PROG_WORKING_ADA
AM_CROSS_PROG_WORKING_ADA
AM_PROG_GNATMAKE
AM_CROSS_PROG_GNATMAKE
AM_CROSS_PROG_CC
AM_PROG_IDLCPP
AM_WITH_GPRBUILD

##########################################
# Check Ada environment features
##########################################

AM_HAS_GNAT_PERFECT_HASH_GENERATORS
AM_HAS_PRAGMA_PROFILE_RAVENSCAR
AM_HAS_PRAGMA_PROFILE_WARNINGS
AM_HAS_PRAGMA_SUPPRESS_VALIDITY_CHECK
AM_HAS_STYLESW_YG
AM_HAS_STYLESW_YO
AM_SUBST_GPR([STYLE_SWITCHES])
AM_HAS_INTRINSIC_SYNC_COUNTERS
AM_HAS_ADA_DYNAMIC_PRIORITIES
AM_HAS_FREE_ON_TERMINATION

##########################################
# Check C environment features (for target)
##########################################

save_CC="$CC"
CC="$CC_FOR_TARGET"

# Need to provide a non-empty 4th argument to AC_CHECK_HEADERS
# so that a real compilation test is performed using CC, not a
# junk preprocessor test that will use the host preprocessor
# instead of the target compiler.

# Mandatory features

AC_CHECK_HEADERS([fcntl.h],
 [],
 [AC_ERROR([One or more required header file could not be found.])],
 [/*relax*/])

# Optional features

AC_CHECK_FUNCS([setsid strftime])
CC="$save_CC"

##########################################
# Check platform properties
##########################################

AC_EXEEXT

##########################################
# Tools list: contains libraries that are
# needed by some personnalities.
##########################################

LIBS_LIST=""

##########################################
# Protocol personalities list.
##########################################

AC_MSG_CHECKING([default protocol personalities])
AC_ARG_WITH(proto-perso,
AS_HELP_STRING([--with-proto-perso=X], [Enumerate protocol personalities]),
[
  PROTO_LIST=""

  newwithval=`echo ${withval} | tr "[A-Z]" "[a-z]"`
  for P in ${newwithval}
  do
     if ${FIND} ${srcdir}/src/${P} -name 'polyorb-binding_data-*.ads' > /dev/null;
     then
        PROTO_LIST="${PROTO_LIST}${P} "
     else
        AC_MSG_ERROR([unknown protocol personality ${P}])
     fi
  done
  PROTO_LIST=`echo "$PROTO_LIST" | sed 's/ $//'`
],
[
  PROTO_LIST="giop"
])
AC_MSG_RESULT(${PROTO_LIST})
AC_SUBST(PROTO_LIST)

##########################################
# Application personalities list
##########################################

AC_MSG_CHECKING([default application personalities])
AC_ARG_WITH(appli-perso,
AS_HELP_STRING([--with-appli-perso=X], [Enumerate application personalities]),
[
  APPLI_LIST=""

  newwithval=`echo ${withval} | tr "[A-Z]" "[a-z]"`
  for P in ${newwithval}
  do
     if test -d "${srcdir}/src/${P}"
     then
        APPLI_LIST="${APPLI_LIST}${P} "
     else
        AC_MSG_ERROR([unknown application personality ${P}])
     fi
  done
  APPLI_LIST=`echo "$APPLI_LIST" | sed 's/ $//'`
],
[
  APPLI_LIST="corba"
])
AC_MSG_RESULT(${APPLI_LIST})
AC_SUBST(APPLI_LIST)

############################################
# Internal configuration of the IDL compiler
############################################

AC_ARG_WITH(idl-compiler,
AS_HELP_STRING([--with-idl-compiler=X], [Select IDL compiler]),
[
  IDLAC=${withval}
],
[
  IDLAC=iac
])
AC_SUBST(IDLAC)

AC_ARG_ENABLE(idlac-wrapper,
AS_HELP_STRING([--disable-idlac-wrapper], [Do not use idlac_wrapper for build]),
[case "$enableval" in
   yes|no) USE_IDLAC_WRAPPER=$enableval ;;
   *)      AC_ERROR([invalid argument to --enable-idlac-wrapper: $enableval]) ;;
esac],
[USE_IDLAC_WRAPPER=yes])
AC_SUBST(USE_IDLAC_WRAPPER)

AC_ARG_ENABLE(use-installed,
AS_HELP_STRING([--enable-use-installed], [Use installed PolyORB to build testsuite and examples]),
[case "$enableval" in
   yes|no) USE_INSTALLED=$enableval ;;
   *)      AC_ERROR([invalid argument to --enable-use-installed: $enableval]) ;;
esac],
[USE_INSTALLED=no])
AC_SUBST(USE_INSTALLED)

AM_IDLCPP_NEEDS_DOT

##########################################
# Services list.
##########################################

AC_MSG_CHECKING([default services])
AC_ARG_WITH(corba-services,
AS_HELP_STRING([--with-corba-services=X], [Enumerate services]),
[
  SERVICE_LIST=""

  newwithval=`echo ${withval} | tr "[A-Z]" "[a-z]"`
  for S in ${newwithval}
  do
     if test -d "${srcdir}/cos/${S}"
     then
        SERVICE_LIST="${SERVICE_LIST}${S} "
     else
        AC_MSG_ERROR([unknown service ${S}])
     fi
  done
  SERVICE_LIST=`echo "$SERVICE_LIST" | sed 's/ $//'`
],
[
  SERVICE_LIST=" "
])
AC_MSG_RESULT(${SERVICE_LIST})
AC_SUBST(SERVICE_LIST)

##########################################
# If the SOAP or the AWS personnality is
# selected, then we have to add the
# WEB_COMMON library
##########################################

soap_enabled=`echo ${PROTO_LIST} | ${AWK} '/soap/ {print "yes"}'`
aws_enabled=`echo ${APPLI_LIST} | ${AWK} '/aws/ {print "yes"}'`

if test x"$soap_enabled" = xyes -o x"$aws_enabled" = xyes
then
   AC_MSG_CHECKING([tools libraries])
   LIBS_LIST="${LIBS_LIST} web_common"
fi

AC_MSG_RESULT(${LIBS_LIST})
AC_SUBST(LIBS_LIST)

##########################################
# gnatdist flags 
##########################################

PCSNAME="polyorb"
AC_SUBST(PCSNAME)

#
# Remote shell command
#
AC_CHECK_PROGS(DEFAULT_RSH_CMD, [remsh rsh ssh], rsh)
AC_MSG_CHECKING(for remote shell command to use)
AC_ARG_WITH(rshcmd,
 AS_HELP_STRING([--with-rshcmd=command], [Set alternate remote shell command]),
 [RSH_CMD="${withval}"],
 [RSH_CMD="${DEFAULT_RSH_CMD}"])
AC_SUBST(RSH_CMD)
AC_MSG_RESULT([${RSH_CMD}])

#
# Remote shell flags
#
AC_MSG_CHECKING(for remote shell options to use)
AC_ARG_WITH(rshopt,
 AS_HELP_STRING([--with-rshopt=command], [Set remote shell options]),
 [RSH_OPT="${withval}"],
 [RSH_OPT=""])
AC_SUBST(RSH_OPT)
AC_MSG_RESULT([${RSH_OPT}])

#
# Default storage location.
#
DEFSTORAGENAME="dsm"
DEFSTORAGEDATA=""
AC_SUBST(DEFSTORAGENAME)
AC_SUBST(DEFSTORAGEDATA)

DEFPROTOCOLNAME="tcp"
DEFPROTOCOLDATA=""
AC_SUBST(DEFPROTOCOLDATA)
AC_SUBST(DEFPROTOCOLNAME)

#
# Check whether we remove support for RPC abortion
#
AC_ARG_ENABLE(rpc-abortion,
AS_HELP_STRING([--enable-rpc-abortion], [Enable RPC abortion even when not supported]),
[SUPPORT_RPC_ABORTION="True"],
[SUPPORT_RPC_ABORTION="Default"])
AM_SUPPORT_RPC_ABORTION
AC_MSG_CHECKING(whether we support RPC abortion)
if test "$SUPPORT_RPC_ABORTION" = "True"; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi
AC_SUBST(SUPPORT_RPC_ABORTION)

##########################################
# Check for exception model
##########################################

AC_MSG_CHECKING(for exception model to use)
AC_MSG_RESULT($EXCEPTION_MODEL)
AC_SUBST(GNAT_RTS_FLAG)

#
# If ATC or dynamic priorities are not supported, remove corresponding units.
# Note: can't use a multi-line value for an AC_SUBST variable, this works only
# with recent versions of autoconf (which use awk(1) for all substitutions).
# Older versions that rely on sed(1) don't handle multi-line substitutions
# correctly.
#

EXCLUDED_SOURCE_FILES=config.excluded_source_files
rm -f $EXCLUDED_SOURCE_FILES

if $HAVE_ADA_ATC; then
  : No excluded files
else
  cat <<__EOF__
polyorb-tasking-profiles-full_tasking_atc.ads
polyorb-tasking-profiles-full_tasking_atc-abortables.ads
polyorb-tasking-profiles-full_tasking_atc-abortables.adb
__EOF__
fi >> ${EXCLUDED_SOURCE_FILES}

if $HAVE_ADA_DYNAMIC_PRIORITIES; then
  : No excluded files
else
  cat <<__EOF__
polyorb-tasking-profiles-full_tasking-threads-dynamic_priorities.ads
polyorb-tasking-profiles-full_tasking-threads-dynamic_priorities.adb
__EOF__
fi >> ${EXCLUDED_SOURCE_FILES}

##########################################
# XML/Ada
##########################################

dnl Projects-based build infrastructure: find XML/Ada project in project path

soap_enabled=`echo ${PROTO_LIST} | ${AWK} '/soap/ {print "yes"}'`

if test x"$soap_enabled" = xyes; then
  AM_HAS_GNAT_PROJECT([xmlada])
  if test x"$HAVE_GNAT_PROJECT_xmlada" != xyes; then
    AC_MSG_ERROR(["SOAP protocol personality requires XML/Ada"])
  fi
fi

##########################################
# GNATColl
##########################################

AM_WITH_GNATCOLL

if test "x$HAVE_GNAT_PROJECT_gnatcoll" = xyes; then
  PLATFORM_BASE_LIST="$PLATFORM_BASE_LIST PolyORB.Parameters.Macros"
  EXTRA_PRJS="$EXTRA_PRJS gnatcoll"
else
  cat <<__EOF__
polyorb-parameters-macros.ads
polyorb-parameters-macros.adb
__EOF__
fi >> ${EXCLUDED_SOURCE_FILES}

##########################################
# SSL/TLS Support with OpenSSL
##########################################

AM_WITH_OPENSSL
AM_CONDITIONAL(HAVE_SSL, [test "x$HAVE_SSL" = "xyes"])

##########################################
# Identify projects to be built
##########################################

get_perso_prjs() {
(
  cd ${srcdir}/projects

  # Look for projects with given personality prefix (or "setup"). Note that
  # these projects may be generated by configure, so consider .gpr.in as well.

  for p in polyorb_src_$1*.gpr*; do

    case "$p" in
      *.gpr)
        # If configure has been run for building in the source tree, then a
        # stale .gpr file might exist for a generated project: ignore it, we'll
        # process the .gpr.in file instead.

        if test -f $p.in; then continue; fi ;;

      *.gpr.in)
        # Process normally

        ;;
      *)
        # Neither .gpr nor .gpr.in: ignore

        continue
    esac

    output=true
    for var in `sed -n '/^--# *require */s///p' < $p`; do
      eval "val=\${$var}"
      if test "x${val}" != "xyes"; then
        output=false
      fi
    done
    if $output; then
      echo -n " ${p%.in}"
    fi
  done
)
}

with_projects() {
  WITH_PRJS=
  case "$1" in
    --prefix=*) prj_prefix=${1#--prefix=}; shift ;;
  esac
  if test x"$1" != x; then
    for gpr in $*; do
      prj="\"${prj_prefix}`basename $gpr .gpr`\""
      if test "x$WITH_PRJS" = x; then
        WITH_PRJS="with $prj"
      else
        WITH_PRJS="$WITH_PRJS, $prj"
      fi
    done
    WITH_PRJS="$WITH_PRJS;"
  fi
  echo "$WITH_PRJS"
}

APPLI_DIRS=""
APPLI_INCS=""
APPLI_LIBS=""
APPLI_EXES="\$(poly_exe) "
APPLI_PRJS=""

for P in ${APPLI_LIST}
do
   APPLI_DIRS="${APPLI_DIRS} "'$'"(${P}_dir)"
   APPLI_INCS="${APPLI_INCS} "'$'"(${P}_inc)"
   APPLI_LIBS="${APPLI_LIBS} "'$'"(${P}_lib)"
   APPLI_EXES="${APPLI_EXES} "'$'"(${P}_exe)"
   APPLI_PRJS="${APPLI_PRJS} `get_perso_prjs ${P}`"
done

WITH_APPLI_PRJS=`with_projects $APPLI_PRJS`

AC_SUBST(APPLI_DIRS)
AC_SUBST(APPLI_INCS)
AC_SUBST(APPLI_LIBS)
AC_SUBST(APPLI_EXES)
AC_SUBST(APPLI_PRJS)
AC_SUBST(WITH_APPLI_PRJS)

PROTO_DIRS=""
PROTO_INCS=""
PROTO_LIBS=""
PROTO_PRJS=""

for P in ${PROTO_LIST}
do
   PROTO_DIRS="${PROTO_DIRS} "'$'"(${P}_dir)"
   PROTO_INCS="${PROTO_INCS} "'$'"(${P}_inc)"
   PROTO_LIBS="${PROTO_LIBS} "'$'"(${P}_lib)"
   PROTO_PRJS="${PROTO_PRJS} `get_perso_prjs ${P}`"
done

WITH_PROTO_PRJS=`with_projects $PROTO_PRJS`

AC_SUBST(PROTO_DIRS)
AC_SUBST(PROTO_INCS)
AC_SUBST(PROTO_LIBS)
AC_SUBST(PROTO_PRJS)
AC_SUBST(WITH_PROTO_PRJS)

SERVICE_DIRS=""
SERVICE_INCS=""
SERVICE_LIBS=""
SERVICE_EXES=""
SERVICE_PRJS=""

for P in ${SERVICE_LIST}
do
   SERVICE_DIRS="${SERVICE_DIRS} "'$'"(${P}_dir)"
   SERVICE_INCS="${SERVICE_INCS} "'$'"(${P}_inc)"
   SERVICE_LIBS="${SERVICE_LIBS} "'$'"(${P}_lib)"
   SERVICE_EXES="${SERVICE_EXES} "'$'"(${P}_exe)"
   if test "$P" != ir
   then
      SERVICE_PRJS="${SERVICE_PRJS} polyorb_idls_cos_${P}.gpr"
      SERVICE_PRJS="${SERVICE_PRJS} polyorb_cos_${P}.gpr"
   fi
done

WITH_SERVICE_PRJS=`with_projects $SERVICE_PRJS`

AC_SUBST(SERVICE_DIRS)
AC_SUBST(SERVICE_INCS)
AC_SUBST(SERVICE_LIBS)
AC_SUBST(SERVICE_EXES)
AC_SUBST(SERVICE_PRJS)
AC_SUBST(WITH_SERVICE_PRJS)

SETUP_PRJS=`get_perso_prjs setup`
WITH_SETUP_PRJS=`with_projects $SETUP_PRJS`
WITH_DISTRIB_SETUP_PRJS=`with_projects --prefix=polyorb/ $SETUP_PRJS`

AC_SUBST(SETUP_PRJS)
AC_SUBST(WITH_SETUP_PRJS)
AC_SUBST(WITH_DISTRIB_SETUP_PRJS)

LIBS_DIRS=""
LIBS_INCS=""
LIBS_LIBS=""
LIBS_EXES=""

for P in ${LIBS_LIST}
do
   LIBS_DIRS="${LIBS_DIRS} "'$'"(${P}_dir)"
   LIBS_INCS="${LIBS_INCS} "'$'"(${P}_inc)"
   LIBS_LIBS="${LIBS_LIBS} "'$'"(${P}_lib)"
   LIBS_EXES="${LIBS_EXES} "'$'"(${P}_exe)"
done

AC_SUBST(LIBS_DIRS)
AC_SUBST(LIBS_INCS)
AC_SUBST(LIBS_LIBS)
AC_SUBST(LIBS_EXES)

##########################################
# Extra gnatmake parameters provided by the user
##########################################

AC_SUBST(EXTRA_GNATMAKE_FLAGS)

#################################################
# Build code fragments to be substituted in
#       src/setup/polyorb-setup-client_base.adb
#       src/setup/polyorb-setup-server.adb
#       tools/po_catref/po_catref_setup.adb
#       tools/po_createref/po_createref_setup.adb
#################################################

PLATFORM_BASE_WITHS=config.platform_base_withs.adb
PROTO_CLIENT_WITHS=config.proto_client_withs.adb
PROTO_SERVER_WITHS=config.proto_server_withs.adb
PO_CATREF_WITHS=config.po_catref_withs.adb
PO_CREATEREF_WITHS=config.po_createref_withs.adb

# Dependency files related to protocol personalities

ALL_PROTO_WITHS="
  ${PROTO_CLIENT_WITHS}
  ${PROTO_SERVER_WITHS}
  ${PO_CATREF_WITHS}
  ${PO_CREATEREF_WITHS}"

# All generated dependency files

ALL_WITHS="
  ${PLATFORM_BASE_WITHS}
  ${ALL_PROTO_WITHS}"

rm -f ${ALL_WITHS}

if test -z "${PLATFORM_BASE_LIST}"
then
  # Generate a comment line to avoid consecutive white lines
  # in the resulting file.

  cat >>${PLATFORM_BASE_WITHS} <<EOF

--  No additional platform-specific dependencies on $target
EOF
else
  for U in ${PLATFORM_BASE_LIST}
  do
    cat >>${PLATFORM_BASE_WITHS} <<EOF

with ${U};
pragma Elaborate_All (${U});
pragma Warnings (Off, ${U});
EOF
  done
fi

for P in ${PROTO_LIST}
do
   c1=`echo "-- ${P} --" | tr a-z A-Z`
   c2=`echo "$c1" | sed s/./-/g`

   for f in ${ALL_PROTO_WITHS}; do
   cat >>${f} <<EOF

$c2
$c1
$c2
EOF
   done

  for F in `${FIND} ${srcdir}/src/${P} -type f \
          | ${GREP} 'polyorb-setup-[[a-z]]*\.ads$' \
  	| ${GREP} -v polyorb-setup-tlsiop.ads`
  do
     U=`${AWK} '/^package/{print $2}' ${F}`
     cat >>${PROTO_SERVER_WITHS} <<EOF

with ${U};
pragma Elaborate_All (${U});
pragma Warnings (Off, ${U});
EOF
     cat >>${PROTO_CLIENT_WITHS} <<EOF

with ${U};
pragma Elaborate_All (${U});
pragma Warnings (Off, ${U});
EOF
  
  done
  
  for F in `${FIND} ${srcdir}/src/${P} -name 'polyorb-binding_data-*.ads'`
  do
     U=`${AWK} '/^package/{print $2}' ${F}`
     cat >>${PROTO_CLIENT_WITHS} <<EOF

with ${U};
pragma Elaborate_All (${U});
pragma Warnings (Off, ${U});
EOF
  done
  
  for F in `${FIND} ${srcdir}/src/${P} -name 'polyorb-setup-access_points-*.ads' \
  	| ${GREP} -v polyorb-setup-access_points-tlsiop.ads`
  do
     U=`${AWK} '/^package/{print $2}' ${F}`
     cat >>${PROTO_SERVER_WITHS} <<EOF

with ${U};
pragma Elaborate_All (${U});
pragma Warnings (Off, ${U});
EOF
  done
  
  for F in `${FIND} ${srcdir}/tools/po_catref -name "polyorb-binding_data-${P}-*.ads"`
  do
     U=`${AWK} '/^package/{print $2}' ${F}`
     cat >>${PO_CATREF_WITHS} <<EOF

with ${U};
pragma Elaborate_All (${U});
pragma Warnings (Off, ${U});
EOF
  done
  
  for F in `${FIND} ${srcdir}/tools/po_createref -name "polyorb-binding_data-${P}-*.ads"`
  do
     U=`${AWK} '/^package/{print $2}' ${F}`
     cat >>${PO_CREATEREF_WITHS} <<EOF

with ${U};
pragma Elaborate_All (${U});
pragma Warnings (Off, ${U});
EOF
  done

done

##########################################
# Clean up files
##########################################

for F in ${ALL_WITHS}
do
   # Remove initial blank line

   SEDCMD="${SED} -e 1d"

   # If we do not have SSL enabled, comment out any mention of SSLIOP
   # in the setup files.

   if test "x$HAVE_SSL" != xyes; then
      SEDCMD=["${SEDCMD} -e 's/[^ ].*SSLIOP/--  &/'"]
   fi

   if test -e ${F}; then 
     eval "${SEDCMD}" < ${F} > ${F}.new
     mv ${F}.new ${F}
   fi
done

AC_SUBST_FILE(PLATFORM_BASE_WITHS)
AC_SUBST_FILE(PROTO_CLIENT_WITHS)
AC_SUBST_FILE(PROTO_SERVER_WITHS)
AC_SUBST_FILE(PO_CATREF_WITHS)
AC_SUBST_FILE(PO_CREATEREF_WITHS)

#################################################
# Consistency checks
#################################################

# Shared libs build requires gprbuild

AS_IF(test "$HAVE_GPRBUILD:${enable_shared:-no}" = "no:yes",
 [AC_MSG_FAILURE("--enable-shared requires gprbuild")])

# COS require CORBA

corba_enabled=`echo ${APPLI_LIST} | ${AWK} '/corba/{print "yes"}'`
AS_IF(test x"$SERVICE_LIST" != x" " -a x$corba_enabled != xyes,
 [AC_MSG_FAILURE("CORBA COS require CORBA application personality")])

#################################################
# Build code fragments to be substituted in
#   Makefile
#################################################

LOCAL_MAKEFILES=config.local.makefiles

(
echo "test_targets :="
(cd ${srcdir} && find . -name Makefile.local) | sed "s,^\./,," | \
while read mklocal; do
  testdir="`dirname ${mklocal}`/"
  echo "current_dir  := ${testdir}"
  echo "test_target  := ${testdir}build-test"
  echo "include \$(top_srcdir)/${mklocal}"
  echo "test_targets += \${test_target}"
done) > ${LOCAL_MAKEFILES}
AC_SUBST_FILE(LOCAL_MAKEFILES)

# GNATDIST version
# This is set differently for the GLADE and PolyORB distributions, which
# share the same source tree.

GNATDIST_VERSION="$POLYORB_VERSION"
AC_SUBST(GNATDIST_VERSION)

##########################################
# Last orders
##########################################

AC_SUBST_FILE(EXCLUDED_SOURCE_FILES)

WITH_EXTRA_PRJS=`with_projects $EXTRA_PRJS`
AC_SUBST(WITH_EXTRA_PRJS)

##########################################
# Output generated files
##########################################

AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_LINKS([src/polyorb-smart_pointers-sync_counters.adb:src/polyorb-smart_pointers-sync_counters__$SYNC_COUNTERS_IMPL.adb
                 testsuite/projects/polyorb_config.gpr:projects/polyorb_config.gpr
                 testsuite/projects/polyorb_common.gpr:projects/polyorb_common.gpr])


AC_OUTPUT(FILTER_OUTPUT_FILES(OUTPUT_FILTER,[
	Makefile
	Makefile.common
	Makefile.common.project
	compilers/common_files/platform.ads
	compilers/config.adc
	compilers/gnatdist/xe_defs-defaults.ads
	compilers/gnatprfh/gnatprfh.adb
	doc/Makefile
	doc/polyorb_version.py
	projects/polyorb.gpr
	projects/polyorb_config.gpr
	projects/polyorb_src_setup.gpr
	projects-distrib/polyorb.gpr
	projects-distrib/polyorb/polyorb_install_common.gpr
	projects-distrib/polyorb/polyorb_src_setup.gpr
	src/config.adc
	src/src.exclude
	src/setup/polyorb-setup-base.adb
	src/setup/polyorb-setup-client_base.adb
	src/setup/polyorb-setup-server.adb
	src/setup/polyorb-setup-tasking-full_tasking.adb
	src/polyorb-platform.ads
	src/polyorb-platform-ssl_linker_options.ads
	src/ravenscar.adc
	src/ravenscar_compatible.adc
	contrib/idlac_wrapper/idlac_wrapper
	examples/corba/secure_echo/gssup.conf
	examples/corba/secure_echo/tls.conf
	examples/corba/secure_echo/tls_gssup.conf
	testsuite/tests/config.py
	tools/po_catref/po_catref_setup.adb
	tools/po_createref/po_createref_setup.adb
	polyorb-config
]),
[
##########################################
# Copy files
##########################################

mkdir support 2> /dev/null
for a in support/gentexifile \
  support/move-if-change contrib/idlac_wrapper/idlac_wrapper
do
  echo "==> updating $a"
  if test ! -f $a; then
    ${RM} -f $a 2> /dev/null
    cp ${srcdir}/$a $a
  fi
  chmod a+x $a
done

chmod a+x polyorb-config
],
[
RM="${RM}"
LN_S="${LN_S}"
${SYMLINK_ALIAS}
])
