AC_PREREQ(2.57)
AC_INIT(polyorb, 2.3.0w, polyorb-bugs@lists.adacore.com)
AC_CONFIG_SRCDIR(src/polyorb.ads)
AC_CONFIG_AUX_DIR(support)

# $Id$

##########################################
# Initialization.
##########################################

AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE
LIBVERSIONINFO=1:4:0
AC_SUBST(LIBVERSIONINFO)

##########################################
# Check for maintainer (debug) mode.
# Note, this must come before AC_PROG_CC
# so we have a chance to set defaults for
# CFLAGS.
##########################################

define(DEBUG_OPTIONS, [dnl
  if test ${CFLAGS-unset} = "unset"; then CFLAGS="-g -O0"; fi
  DEFAULT_ADAFLAGS="-XBuild=DEBUG"
  BARGS=-E
  DEBUG_ONLY=""
  debug=true])

define(NODEBUG_OPTIONS, [dnl
  DEFAULT_ADAFLAGS="-XBuild=PRODUCTION"
  BARGS=
  DEBUG_ONLY="--  "
  debug=false])

AC_ARG_ENABLE(debug,
[  --enable-debug          Turn on debugging options],
[if [[ "$enableval" = "yes" ]]; then
  DEBUG_OPTIONS
else
  NODEBUG_OPTIONS
fi],
[NODEBUG_OPTIONS])

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
  ADAFLAGS="${CFLAGS} ${DEFAULT_ADAFLAGS}"
fi

AC_PROG_INSTALL
AC_PROG_AWK
AC_CHECK_PROG(RM, rm, rm)
AC_CHECK_PROG(SED, sed, sed)
AC_CHECK_PROG(FIND, find, find)
AC_CHECK_PROG(PYTHON, python, python)
AC_CHECK_PROG(GREP, grep, grep)
AM_PROG_GNATCHOP
AM_PROG_ADA
AM_CROSS_PROG_ADA
AM_PROG_WORKING_ADA
AM_CROSS_PROG_WORKING_ADA
AM_PROG_GNATMAKE
AM_CROSS_PROG_GNATMAKE
AM_CROSS_PROG_CC
AM_PROG_IDLCPP
AM_PROG_XARGS_I

##########################################
# Check Ada environment features
##########################################

AM_HAS_GNAT_PERFECT_HASH_GENERATORS
AM_HAS_PRAGMA_PROFILE_RAVENSCAR
AM_HAS_PRAGMA_PROFILE_WARNINGS
AM_HAS_PRAGMA_SUPPRESS_VALIDITY_CHECK
AM_HAS_STYLESW_YG

##########################################
# Check platform properties
##########################################

AC_EXEEXT
AM_ENABLE_SHARED(no)
AM_ENABLE_STATIC(yes)

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
[  --with-proto-perso=x    Enumerate protocol personalities],
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
[  --with-appli-perso=x    Enumerate application personalities],
[
  APPLI_LIST=""

  newwithval=`echo ${withval} | tr "[A-Z]" "[a-z]"`
  for P in ${newwithval}
  do
     if test -d ${srcdir}/src/${P}
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
[  --with-idl-compiler=x   Internal configuration flag],
[
  IDLAC=${withval}
],
[
  IDLAC=idlac
])
AC_SUBST(IDLAC)
AM_IDLCPP_NEEDS_DOT

##########################################
# Services list.
##########################################

AC_MSG_CHECKING([default services])
AC_ARG_WITH(corba-services,
[  --with-corba-services=x       Enumerate services],
[
  SERVICE_LIST=""

  newwithval=`echo ${withval} | tr "[A-Z]" "[a-z]"`
  for S in ${newwithval}
  do
     if test -f ${srcdir}/cos/${S}/Makefile.am
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
# Update Makefiles' subdirectory lists
##########################################

get_perso_prjs() {
  perso_kind=$1
  perso_name=$2
  result=
  sep=""
  for gpr in ${srcdir}/projects/src_${perso_name}*.gpr; do
    result="${result}${sep}\"`basename $gpr .gpr`\""
    sep=","
  done
  echo $result
}

APPLI_DIRS=""
APPLI_EXES="\$(poly_exe) "
APPLI_INCS=""
APPLI_LIBS=""

sep=""
for P in ${APPLI_LIST}
do
   APPLI_DIRS="${APPLI_DIRS} "'$'"(${P}_dir)"
   APPLI_LIBS="${APPLI_LIBS} "'$'"(${P}_lib)"
   APPLI_INCS="${APPLI_INCS} "'$'"(${P}_inc)"
   APPLI_EXES="${APPLI_EXES} "'$'"(${P}_exe)"
   APPLI_PRJS="${APPLI_PRJS}${sep}`get_perso_prjs appli ${P}`"
   sep=","
done

AC_SUBST(APPLI_DIRS)
AC_SUBST(APPLI_INCS)
AC_SUBST(APPLI_LIBS)
AC_SUBST(APPLI_PRJS)
AC_SUBST(APPLI_EXES)
# ??? Do we have any APPLI_EXES?

PROTO_DIRS=""
PROTO_INCS=""
PROTO_LIBS=""

sep=""
for P in ${PROTO_LIST}
do
   PROTO_DIRS="${PROTO_DIRS} "'$'"(${P}_dir)"
   PROTO_INCS="${PROTO_INCS} "'$'"(${P}_inc)"
   PROTO_LIBS="${PROTO_LIBS} "'$'"(${P}_lib)"
   PROTO_PRJS="${PROTO_PRJS}${sep}`get_perso_prjs proto ${P}`"
   sep=","
done

AC_SUBST(PROTO_DIRS)
AC_SUBST(PROTO_INCS)
AC_SUBST(PROTO_LIBS)
AC_SUBST(PROTO_PRJS)

SERVICE_DIRS=""
SERVICE_INCS=""
SERVICE_LIBS=""
SERVICE_EXES=""

for P in ${SERVICE_LIST}
do
   SERVICE_DIRS="${SERVICE_DIRS} "'$'"(${P}_dir)"
   SERVICE_INCS="${SERVICE_INCS} "'$'"(${P}_inc)"
   SERVICE_LIBS="${SERVICE_LIBS} "'$'"(${P}_lib)"
   SERVICE_EXES="${SERVICE_EXES} "'$'"(${P}_exe)"
done

AC_SUBST(SERVICE_DIRS)
AC_SUBST(SERVICE_INCS)
AC_SUBST(SERVICE_LIBS)
AC_SUBST(SERVICE_EXES)

LIBS_DIRS=""
LIBS_EXES=""
LIBS_INCS=""
LIBS_LIBS=""

for P in ${LIBS_LIST}
do
   LIBS_DIRS="${LIBS_DIRS} "'$'"(${P}_dir)"
   LIBS_LIBS="${LIBS_LIBS} "'$'"(${P}_lib)"
   LIBS_INCS="${LIBS_INCS} "'$'"(${P}_inc)"
   LIBS_EXES="${LIBS_EXES} "'$'"(${P}_exe)"
done

AC_SUBST(LIBS_DIRS)
AC_SUBST(LIBS_EXES)
AC_SUBST(LIBS_INCS)
AC_SUBST(LIBS_LIBS)

##########################################
# gnatdist flags 
##########################################

PCSNAME="polyorb"
AC_SUBST(PCSNAME)

#
# Remote shell command
#
AC_MSG_CHECKING(for remote shell command to use)
AC_ARG_WITH(rshcmd,
            [  --with-gnatdist-rshcmd=command   Set alternate remote shell command],
            [RSH_CMD="${withval}"],
            [RSH_CMD="rsh"])
AC_SUBST(RSH_CMD)
AC_MSG_RESULT([${RSH_CMD}])

#
# Remote shell flags
#
AC_MSG_CHECKING(for remote shell options to use)
AC_ARG_WITH(rshopt,
            [  --with-gnatdist-rshopt=command   Set remote shell options],
            [RSH_OPT="${withval}"],
            [RSH_OPT="-f"])
AC_SUBST(RSH_OPT)
AC_MSG_RESULT([${RSH_OPT}])

#
# Not used by polyORB, set default values for garlic.
#
DEFSTORAGENAME="dfs"
DEFSTORAGEDATA=""
AC_SUBST(DEFSTORAGENAME)
AC_SUBST(DEFSTORAGEDATA)

DEFPROTOCOLNAME="tcp"
DEFPROTOCOLDATA=""
AC_SUBST(DEFPROTOCOLDATA)
AC_SUBST(DEFPROTOCOLNAME)

#
# Check for optimization flags
# 
AC_ARG_WITH(optimization,
            [  --with-gnatdist-optimization=X   Use X as default optimization for gnatdist (O0)],
            [GNATDISTOPT="$withval"],
            [GNATDISTOPT="O0"])
AC_SUBST(GNATDISTOPT)

#
# Check whether we remove support for RPC abortion
#
AC_ARG_ENABLE(rpc-abortion,
[  --enable-rpc-abortion   Enable RPC abortion even when not supported],
[SUPPORT_RPC_ABORTION="True"],
[SUPPORT_RPC_ABORTION="Default"])
AM_SUPPORT_RPC_ABORTION
AC_MSG_CHECKING(whether we support RPC abortion)
if test "$SUPPORT_RPC_ABORTION" = "True"; then
  AC_MSG_RESULT([true])
else
  AC_MSG_RESULT([false])
fi
AC_SUBST(SUPPORT_RPC_ABORTION)

#
# Check for exception model
#
AC_MSG_CHECKING(for exception model to use)
AC_MSG_RESULT($EXCEPTION_MODEL)
AC_SUBST(GNAT_RTS_FLAG)

##########################################
# XML/Ada
##########################################

XMLADA_INCS=""
XMLADA_LIBS=""
has_xmlada=no

dnl Legacy build infrastructure: find XmlAda using xmlada-config

AC_MSG_CHECKING([XmlAda])
if xmlada-config --version 2>&1 | ${GREP} "^XmlAda" > /dev/null 2>&1; then
   XMLADA_INCS="`xmlada-config --cflags`"
   XMLADA_LIBS="`xmlada-config --libs`"
   has_xmlada=yes
else
   has_xmlada=no
fi
AC_MSG_RESULT($has_xmlada)
AM_CONDITIONAL(HAS_XMLADA, test x"$has_xmlada" = xyes)

dnl Projects-based build infrastructure: find XmlAda project in project path
dnl AM_HAS_GNAT_PROJECT([xmlada])
dnl AM_CONDITIONAL(HAS_XMLADA, test x"$HAVE_GNAT_PROJECT_xmlada" = xyes)

soap_enabled=`echo ${PROTO_LIST} | ${AWK} '/soap/ {print "yes"}'`

##########################################
# Test : if SOAP personality is built,
#        then XmlAda must be configured
##########################################

if test x"$soap_enabled" = xyes -a x"$has_xmlada" != xyes
then
   AC_MSG_ERROR("Protocol personality SOAP requires XmlAda")
fi

##########################################
# Update Xml/Ada configuration flags
# iff SOAP is built
##########################################

if test x"$soap_enabled" != xyes
then
   XMLADA_INCS=""
   XMLADA_LIBS=""
fi

AC_SUBST(XMLADA_INCS)
AC_SUBST(XMLADA_LIBS)

##########################################
# SSL/TLS Support with OpenSSL
##########################################

AM_WITH_OPENSSL
AM_CONDITIONAL(HAVE_SSL, [test "x$HAVE_SSL" = "xyes"])

##########################################
# Test : if a CORBA COS is built
#        then CORBA must be built
##########################################

corba_enabled=`echo ${APPLI_LIST} | ${AWK} '/corba/{print "yes"}'`

if test x"$SERVICE_LIST" != x" " -a x$corba_enabled != xyes
then
   AC_MSG_ERROR("CORBA COS require CORBA application personality")
fi

##########################################################
# For AIX, the Interface Repository must be built with -O0
##########################################################

case $target in
  powerpc-ibm-aix*)
    DISABLE_IR_OPTIMIZATION=yes
    ;;
  *)
    DISABLE_IR_OPTIMIZATION=no
    ;;
esac
AM_CONDITIONAL(DISABLE_IR_OPTIMIZATION, [test "x$DISABLE_IR_OPTIMIZATION" = "xyes"])

##########################################
# Extra gnatmake parameters provided by the user
##########################################

AC_SUBST(EXTRA_GNATMAKE_FLAGS)

##########################################
# We change $host in order to force the generation of a libtool for the
# target, and not for the host.
# XXX if someone knows a clean way to do that, be my guest...
##########################################

host_tmp=$host
host=$target
AM_PROG_LIBTOOL
AC_LIBTOOL_HAS_TAG
host=$host_tmp

##########################################
# Windows requires specific targets
##########################################

case "$host_os" in
  cygwin* | mingw*)
    windows_host=true
    HOST_EXE_SUFFIX=.exe
    ;;
  *)
    windows_host=false
    HOST_EXE_SUFFIX=
    ;;
esac
AM_CONDITIONAL(WINDOWS, $windows_host)

case "$target_os" in
  *cygwin* | *mingw*)
    TARGET_EXE_SUFFIX=.exe
    ;;
  *)
    TARGET_EXE_SUFFIX=
    ;;
esac

# Compilers are always built using the native Ada compiler. Therefore,
# their executable suffix depends on the host operating system.

COMPILER_EXE_SUFFIX=$HOST_EXE_SUFFIX
AC_SUBST(COMPILER_EXE_SUFFIX)

# Tools are using the cross Ada compiler. Therefore, their executable
# suffix depends on the target operating system.

TOOL_EXE_SUFFIX=$TARGET_EXE_SUFFIX
AC_SUBST(TOOL_EXE_SUFFIX)

#################################################
# Build code fragments to be substituted in
#       src/setup/polyorb-setup-client.adb
#       src/setup/polyorb-setup-client.adb
#       tools/po_catref/po_catref_setup.adb
#       tools/po_createref/po_createref_setup.adb
#################################################

PROTO_CLIENT_WITHS=config.proto_client_withs.adb
PROTO_SERVER_WITHS=config.proto_server_withs.adb
PO_CATREF_WITHS=config.po_catref_withs.adb
PO_CREATEREF_WITHS=config.po_createref_withs.adb

ALL_WITHS="
  ${PROTO_CLIENT_WITHS}
  ${PROTO_SERVER_WITHS}
  ${PO_CATREF_WITHS}
  ${PO_CREATEREF_WITHS}"

rm -f ${ALL_WITHS}

for P in ${PROTO_LIST}
do
   c1=`echo "-- ${P} --" | tr a-z A-Z`
   c2=`echo "$c1" | sed s/./-/g`

   for f in ${ALL_WITHS}; do
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

AC_SUBST_FILE(PROTO_CLIENT_WITHS)
AC_SUBST_FILE(PROTO_SERVER_WITHS)
AC_SUBST_FILE(PO_CATREF_WITHS)
AC_SUBST_FILE(PO_CREATEREF_WITHS)

##########################################
# Output generated files
##########################################

dnl Important! One file per line, nothing before or after except whitespace!
dnl This section is edited automatically by support/reconfig.

AC_OUTPUT([
	Makefile
	Makefile.common.project
	Makefile.old
	support/Makefile
	compilers/Makefile
	compilers/common_files/platform.ads
	compilers/gnatdist/Makefile
	compilers/gnatdist/xe_defs-defaults.ads
	compilers/gnatprfh/Makefile
	compilers/gnatprfh/gnatprfh.adb
	compilers/iac/Makefile
	compilers/iac/testsuite/Makefile
	compilers/idlac/Makefile
	compilers/idlac/testsuite/Makefile
	docs/Makefile
	projects/config.gpr
	projects/src_setup.gpr
	src/Makefile
	src/corba/Makefile
	src/corba/dynamicany/Makefile
	src/corba/iop/Makefile
	src/corba/messaging/Makefile
	src/corba/portableinterceptor/Makefile
	src/corba/rtcorba/Makefile
	src/corba/security/Makefile
	src/corba/security/gssup/Makefile
	src/dsa/Makefile
	src/giop/Makefile
	src/giop/iiop/Makefile
	src/giop/iiop/security/Makefile
	src/giop/iiop/security/tls/Makefile
	src/giop/iiop/ssliop/Makefile
	src/giop/diop/Makefile
	src/giop/miop/Makefile
	src/moma/Makefile
	src/web_common/Makefile
	src/aws/Makefile
	src/security/Makefile
	src/security/gssup/Makefile
	src/security/tls/Makefile
	src/security/x509/Makefile
	src/setup/Makefile
	src/setup/polyorb-setup-client.adb
	src/setup/polyorb-setup-server.adb
	src/setup/security/Makefile
	src/soap/Makefile
	src/srp/Makefile
	src/ssl/Makefile
	src/debug.adc
	src/polyorb-platform-ssl_linker_options.ads
	src/ravenscar.adc
	src/ravenscar_compatible.adc
	idls/Makefile
	idls/CORBA_IDL/Makefile
	idls/CORBA_PIDL/Makefile
	idls/Interop/Makefile
	idls/Misc/Makefile
	idls/RTCORBA/Makefile
	idls/cos/Makefile
	idls/cos/event/Makefile
	idls/cos/naming/Makefile
	idls/cos/notification/Makefile
	idls/cos/time/Makefile
	contrib/idlac_wrapper/idlac_wrapper
	cos/Makefile
	cos/event/Makefile
	cos/naming/Makefile
	cos/notification/Makefile
	cos/time/Makefile
	cos/ir/Makefile
	examples/Makefile
	examples/corba/Makefile
	examples/corba/all_types/Makefile
	examples/corba/all_functions/Makefile
	examples/corba/echo/Makefile
	examples/corba/random/Makefile
	examples/corba/rtcorba/Makefile
	examples/corba/rtcorba/client_propagated/Makefile
	examples/corba/rtcorba/dhb/Makefile
	examples/corba/rtcorba/server_declared/Makefile
	examples/corba/rtcorba/rtcosscheduling/Makefile
	examples/corba/send/Makefile
	examples/corba/secure_echo/Makefile
	examples/corba/secure_echo/gssup.conf
	examples/corba/secure_echo/gssup_example
	examples/corba/secure_echo/tls.conf
	examples/corba/secure_echo/tls_example
	examples/corba/secure_echo/tls_gssup.conf
	examples/corba/secure_echo/tls_gssup_example
	examples/dsa/Makefile
	examples/bbs/Makefile
	examples/moma/Makefile
	examples/aws/Makefile
	examples/polyorb/Makefile
	testsuite/Makefile
	testsuite/corba/Makefile
	testsuite/corba/all_exceptions/Makefile
	testsuite/corba/benchs/Makefile
	testsuite/corba/benchs/test000/Makefile
	testsuite/corba/code_sets/Makefile
	testsuite/corba/code_sets/test000/Makefile
	testsuite/corba/cos/Makefile
	testsuite/corba/cos/event/Makefile
	testsuite/corba/cos/ir/Makefile
	testsuite/corba/cos/naming/Makefile
	testsuite/corba/cos/notification/Makefile
	testsuite/corba/cos/time/Makefile
	testsuite/corba/domainmanager/Makefile
	testsuite/corba/domainmanager/test000/Makefile
	testsuite/corba/harness/Makefile
	testsuite/corba/local/Makefile
	testsuite/corba/location_forwarding/Makefile
	testsuite/corba/location_forwarding/test000/Makefile
	testsuite/corba/location_forwarding/test001/Makefile
	testsuite/corba/orb_init/Makefile
	testsuite/corba/object/Makefile
	testsuite/corba/object/test000/Makefile
	testsuite/corba/portableinterceptor/Makefile
	testsuite/corba/portableinterceptor/test000/Makefile
	testsuite/corba/portableinterceptor/test001/Makefile
	testsuite/corba/portableinterceptor/test002/Makefile
	testsuite/corba/portableinterceptor/test003/Makefile
	testsuite/corba/portableinterceptor/test004/Makefile
	testsuite/corba/portableserver/Makefile
	testsuite/corba/rtcorba/Makefile
	testsuite/corba/rtcorba/rtcurrent/Makefile
	testsuite/corba/rtcorba/rtorb/Makefile
	testsuite/corba/rtcorba/rtpoa/Makefile
	testsuite/corba/shutdown/Makefile
	testsuite/core/Makefile
	testsuite/core/any/Makefile
	testsuite/core/chained_lists/Makefile
	testsuite/core/dynamic_dict/Makefile
	testsuite/core/fixed_point/Makefile
	testsuite/core/initialization/Makefile
	testsuite/core/naming/Makefile
	testsuite/core/obj_adapters/Makefile
	testsuite/core/poa/Makefile
	testsuite/core/random/Makefile
	testsuite/core/sync_policies/Makefile
	testsuite/core/tasking/Makefile
	testsuite/core/uri_encoding/Makefile
	testsuite/acats/Makefile
	testsuite/acats/CXE1001/Makefile
	testsuite/acats/CXE2001/Makefile
	testsuite/acats/CXE4001/Makefile
	testsuite/acats/CXE4002/Makefile
	testsuite/acats/CXE4005/Makefile
	testsuite/acats/CXE4006/Makefile
	testsuite/utils/Makefile
	tools/Makefile
	tools/po_catref/Makefile
	tools/po_catref/po_catref_setup.adb
	tools/po_cos_naming/Makefile
	tools/po_createref/Makefile
	tools/po_createref/po_createref_setup.adb
	tools/po_dumpir/Makefile
	tools/po_ir/Makefile
	tools/po_names/Makefile
	support/adacompiler
	support/linker
	polyorb-config
],
[
##########################################
# Copy files
##########################################

for a in support/linker support/adacompiler support/gentexifile \
  support/move-if-change contrib/idlac_wrapper/idlac_wrapper \
  examples/corba/secure_echo/gssup_example \
  examples/corba/secure_echo/tls_example \
  examples/corba/secure_echo/tls_gssup_example
do
  echo "==> updating $a"
  if test ! -f $a; then
    ${RM} -f $a 2> /dev/null
    cp ${srcdir}/$a $a
  fi
  chmod a+x $a
done

chmod a+x polyorb-config

for a in adacompiler linker; do
  na=support/native-$a
  echo "==> symlinking $na"
  ${RM} -f support/native-$a
  # The following 'ln -s' command needs to happen in the support subdirectory,
  # so that we can replace it with 'cp' on machines where 'ln -s' doesn't work
  # (such as Windows). That is, 'ln -s $a support/native-$a' would work, but
  # 'cp $a support/native-$a' does not; hence the "cd support...".
  (cd support ; ${LN_S} $a native-$a)
done
],
[
RM="${RM}"
LN_S="${LN_S}"
])
