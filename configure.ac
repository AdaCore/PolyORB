AC_PREREQ(2.57)
AC_INIT(polyorb, 1.2w, polyorb-bugs@lists.act-europe.fr)
AC_CONFIG_SRCDIR(src/polyorb.ads)
AC_CONFIG_AUX_DIR(support)

# $Id$

##########################################
# Initialization.
##########################################

AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE
LIBVERSIONINFO=1:2:0
AC_SUBST(LIBVERSIONINFO)

##########################################
# Check fo various programs.
##########################################

AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_CXXCPP
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
AM_HAS_GNAT_SOCKETS_COPY
AM_HAS_GNAT_OS_LIB_CLOSE_WITH_STATUS
AM_HAS_GNAT_PERFECT_HASH_GENERATORS
AM_HAS_PRAGMA_PROFILE_RAVENSCAR
AM_HAS_PRAGMA_PROFILE_WARNINGS
AC_EXEEXT
AM_CROSS_PROG_CC
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
        PROTO_LIST="${PROTO_LIST} $P "
     else
        AC_MSG_ERROR([unknown protocol personality ${P}])
     fi
  done
],
[
  PROTO_LIST=" giop "
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
     if test -d ${srcdir}/src/${P} -o ${P} = "dsa"
     then
        APPLI_LIST="${APPLI_LIST} $P "
     else
        AC_MSG_ERROR([unknown application personality ${P}])
     fi
  done
],
[
   APPLI_LIST=" corba "
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
if test "$CXXCPPFLAGS" = ""; then
  CXXCPPFLAGS="-C -x c -ansi"
  # Use default options for the GNU CPP:
  # -C           do not discard comments
  # -x c++       use c++ preprocessor semantic
  # FIXME: The GCC3 preprocessor won't accept '-x c++'
  # unless the C++ *compiler* has been built as well.
  # For now, just use '-x c'.
fi
AC_SUBST(CXXCPPFLAGS)
AM_CXXCPP_NEEDS_DOT

##########################################
# Services list.
##########################################

AC_MSG_CHECKING([default services])
AC_ARG_WITH(services,
[  --with-services=x       Enumerate services],
[
  SERVICE_LIST=""

  newwithval=`echo ${withval} | tr "[A-Z]" "[a-z]"`
  for S in ${newwithval}
  do
     if test -f ${srcdir}/cos/${S}/Makefile.am
     then
        SERVICE_LIST="${SERVICE_LIST} $S "
     else
        AC_MSG_ERROR([unknown service ${S}])
     fi
  done
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

soap_enabled=`echo ${PROTO_LIST} | ${AWK} '/soap/{print "yes"}'`
aws_enabled=`echo ${APPLI_LIST} | ${AWK} '/aws/{print "yes"}'`

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

APPLI_DIRS=""
APPLI_EXES="\$(poly_exe) "
APPLI_INCS=""
APPLI_LIBS=""

for P in ${APPLI_LIST}
do
   APPLI_DIRS="${APPLI_DIRS} "'$'"(${P}_dir)"
   APPLI_LIBS="${APPLI_LIBS} "'$'"(${P}_lib)"
   APPLI_INCS="${APPLI_INCS} "'$'"(${P}_inc)"
   APPLI_EXES="${APPLI_EXES} "'$'"(${P}_exe)"
done

AC_SUBST(APPLI_DIRS)
AC_SUBST(APPLI_EXES)
AC_SUBST(APPLI_INCS)
AC_SUBST(APPLI_LIBS)

PROTO_DIRS=""
PROTO_INCS=""
PROTO_LIBS=""

for P in ${PROTO_LIST}
do
   PROTO_DIRS="${PROTO_DIRS} "'$'"(${P}_dir)"
   PROTO_INCS="${PROTO_INCS} "'$'"(${P}_inc)"
   PROTO_LIBS="${PROTO_LIBS} "'$'"(${P}_lib)"
done

AC_SUBST(PROTO_DIRS)
AC_SUBST(PROTO_INCS)
AC_SUBST(PROTO_LIBS)

SERVICE_DIRS=""
SERVICE_INCS=""
SERVICE_EXES=""

for P in ${SERVICE_LIST}
do
   SERVICE_DIRS="${SERVICE_DIRS} "'$'"(${P}_dir)"
   SERVICE_INCS="${SERVICE_INCS} "'$'"(${P}_inc)"
   SERVICE_EXES="${SERVICE_EXES} "'$'"(${P}_exe)"
done

AC_SUBST(SERVICE_DIRS)
AC_SUBST(SERVICE_INCS)
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
# XML/Ada
##########################################

XMLADA_INCS=""
XMLADA_LIBS=""
has_xmlada=no

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

soap_enabled=`echo ${PROTO_LIST} | ${AWK} '/soap/{print "yes"}'`

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
# Test : if a CORBA COS is built
#        then CORBA must be built
##########################################

corba_enabled=`echo ${APPLI_LIST} | ${AWK} '/corba/{print "yes"}'`

if test x"$SERVICE_LIST" != x" " -a x$corba_enabled != xyes
then
   AC_MSG_ERROR("CORBA COS require CORBA application personality")
fi

##########################################
# Check for maintainer (debug) mode.
##########################################

define(DEBUG_OPTIONS, [dnl
  GNATFLAGS="-g -O2 -gnatfy -gnatwae -gnatoa -fstack-check"
  BARGS=-E
  debug=true])
define(NODEBUG_OPTIONS, [dnl
  GNATFLAGS="-g -O2 -gnatfy -gnatwae -gnatpn"
  BARGS=
  debug=false])

AC_ARG_ENABLE(debug,
[  --enable-debug          Turn on debugging options],
[if [[ "$enableval" = "yes" ]]; then
  DEBUG_OPTIONS
else
  NODEBUG_OPTIONS
fi],
[NODEBUG_OPTIONS])

AC_SUBST(GNATFLAGS)
AC_SUBST(BARGS)
AM_CONDITIONAL(DEBUG, test x$debug = xtrue)

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
    windows=true
    ;;
  *)
    windows=false
    ;;
esac
AM_CONDITIONAL(WINDOWS, test x$windows = xtrue)

##########################################
# Build src/polyorb-setup-client.adb
#       src/polyorb-setup-client.adb
#       tools/po_catref/po_catref_setup.adb
##########################################

PROTO_CLIENT_WITHS=${srcdir}/proto_client_withs.adb
echo "" > ${PROTO_CLIENT_WITHS}

PROTO_SERVER_WITHS=${srcdir}/proto_server_withs.adb
echo "" > ${PROTO_SERVER_WITHS}

PO_CATREF_WITHS=${srcdir}/po_catref_withs.adb
echo "" > ${PO_CATREF_WITHS}

for P in ${PROTO_LIST}
do
   cat >>${PROTO_CLIENT_WITHS} <<EOF
-- ${P} --
EOF

   cat >>${PROTO_SERVER_WITHS} <<EOF
-- ${P} --
EOF

for F in `${FIND} ${srcdir}/src/${P} -type f \
        | ${GREP} 'polyorb-setup-[[a-z]]*\.ads$'`
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


for F in `${FIND} ${srcdir}/src/${P} -name 'polyorb-setup-access_points-*.ads'`
do
   U=`${AWK} '/^package/{print $2}' ${F}`
   cat >>${PROTO_SERVER_WITHS} <<EOF
with ${U};
pragma Elaborate_All (${U});
pragma Warnings (Off, ${U});

EOF

done

for F in `${FIND} ${srcdir}/src/${P} -name 'polyorb-binding_data-*.ads'`
do
   U=`${AWK} '/^package/{print $2}' ${F}`
   cat >>${PO_CATREF_WITHS} <<EOF
with ${U}.Print;
pragma Elaborate_All (${U}.Print);
pragma Warnings (Off, ${U}.Print);

EOF

done


done

##########################################
# Clean up files
##########################################

for F in ${PO_CATREF_WITHS} ${PROTO_CLIENT_WITHS} ${PROTO_SERVER_WITHS}
do
   ${SED} -e 's/.*PolyORB.Binding_Data.GIOP.INET.*//g' < ${F} > ${F}.new
   mv ${F}.new ${F}
done

for F in ${PO_CATREF_WITHS}
do
   ${SED} -e 's/.*PolyORB.Binding_Data.GIOP.Print.*//g' < ${F} > ${F}.new
   mv ${F}.new ${F}
done

AC_SUBST_FILE(PROTO_CLIENT_WITHS)
AC_SUBST_FILE(PROTO_SERVER_WITHS)
AC_SUBST_FILE(PO_CATREF_WITHS)

##########################################
# Output generated files
##########################################

dnl Important! One file per line, nothing before
dnl or after except whitespace! This section
dnl is edited automatically by make_distrib.

AC_OUTPUT([
	Makefile
	support/Makefile
	compilers/Makefile
	compilers/gnatprfh/Makefile
	compilers/gnatprfh/gnatprfh.adb
	compilers/iac/Makefile
	compilers/iac/testsuite/Makefile
	compilers/iac/platform.ads
	compilers/idlac/Makefile
	compilers/idlac/testsuite/Makefile
	compilers/idlac/platform.ads
	doc/Makefile
	src/Makefile
	src/corba/Makefile
	src/corba/messaging/Makefile
	src/corba/portableinterceptor/Makefile
	src/corba/rtcorba/Makefile
	src/dsa/Makefile
	src/giop/Makefile
	src/giop/iiop/Makefile
	src/giop/diop/Makefile
	src/giop/miop/Makefile
	src/moma/Makefile
	src/web_common/Makefile
	src/aws/Makefile
	src/soap/Makefile
	src/srp/Makefile
	src/polyorb-setup-client.adb
	src/polyorb-setup-server.adb
	src/polyorb-sockets_copy.adb
        src/ravenscar.adc
        src/ravenscar_compatible.adc
	idls/Makefile
	idls/cos/Makefile
	idls/cos/event/Makefile
	idls/cos/naming/Makefile
	idls/cos/time/Makefile
	cos/Makefile
	cos/event/Makefile
	cos/naming/Makefile
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
	examples/corba/rtcorba/server_declared/Makefile
	examples/corba/send/Makefile
	examples/dsa/Makefile
	examples/bbs/Makefile
	examples/moma/Makefile
	examples/aws/Makefile
	examples/polyorb/Makefile
	testsuite/Makefile
	testsuite/corba/Makefile
	testsuite/corba/all_exceptions/Makefile
	testsuite/corba/code_sets/Makefile
	testsuite/corba/code_sets/test000/Makefile
	testsuite/corba/cos/Makefile
	testsuite/corba/cos/event/Makefile
	testsuite/corba/cos/ir/Makefile
	testsuite/corba/cos/naming/Makefile
	testsuite/corba/cos/time/Makefile
	testsuite/corba/harness/Makefile
	testsuite/corba/local/Makefile
	testsuite/corba/location_forwarding/Makefile
	testsuite/corba/location_forwarding/test000/Makefile
	testsuite/corba/location_forwarding/test001/Makefile
	testsuite/corba/orb_init/Makefile
	testsuite/corba/portableinterceptor/Makefile
	testsuite/corba/portableinterceptor/test000/Makefile
	testsuite/corba/portableinterceptor/test001/Makefile
	testsuite/corba/portableinterceptor/test002/Makefile
	testsuite/corba/portableinterceptor/test003/Makefile
	testsuite/corba/portableserver/Makefile
	testsuite/corba/rtcorba/Makefile
	testsuite/corba/rtcorba/rtcurrent/Makefile
	testsuite/corba/rtcorba/rtorb/Makefile
	testsuite/corba/rtcorba/rtpoa/Makefile
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
	testsuite/core/tasking/Makefile
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
	tools/po_dumpir/Makefile
        tools/po_names/Makefile
	support/adacompiler
	support/linker
	polyorb-config
])

##########################################
# Clean up
##########################################

${RM} ${PROTO_CLIENT_WITHS}
${RM} ${PROTO_SERVER_WITHS}
${RM} ${PO_CATREF_WITHS}

##########################################
# Copy files
##########################################

if test ! -f support/move-if-change; then
  ${RM} support/move-if-change 2> /dev/null
  cp ${srcdir}/support/move-if-change support/move-if-change
fi

for a in linker adacompiler move-if-change
do
  chmod a+x support/$a
done
rm -f support/native-adacompiler
ln -f support/adacompiler support/native-adacompiler