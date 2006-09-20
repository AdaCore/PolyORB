dnl Ada compiler handling
dnl $Id$
dnl Contributed by Samuel Tardieu <sam@inf.enst.fr>

dnl Usage: AM_PROG_ADA
dnl Look for an Ada compiler (ADA environment variable, then gcc, then $CC)

AC_DEFUN([AM_PROG_ADA],
[AC_BEFORE([$0], [AM_TRY_ADA])
AC_REQUIRE([AC_PROG_CC])
AC_CHECK_PROGS(ADA, gnatgcc adagcc gcc)
if test -z "$ADA"; then
  AC_MSG_RESULT([  Tentatively using $CC as an Ada compiler])
  ADA="$CC"
fi])

dnl Usage: AM_PROG_GNATCHOP
dnl Look for GNATCHOP program

AC_DEFUN([AM_PROG_GNATCHOP],
[AC_CHECK_PROG(GNATCHOP, gnatchop, gnatchop)])

dnl Usage: AM_PROG_GNATLS
dnl Look for GNATLS program

AC_DEFUN([AM_PROG_GNATLS],
[AC_CHECK_PROG(GNATLS, gnatls, gnatls)])

dnl Usage: AM_TRY_ADA(gnatmake, filename, content, pragmas, success, failure)
dnl Compile, bind and link an Ada program and report its success or failure

AC_DEFUN([AM_TRY_ADA],
[mkdir conftest
cat > conftest/src.ada <<EOF
[$3]
EOF
cat > conftest/gnat.adc <<EOF
[$4]
EOF
ac_try="cd conftest && $GNATCHOP -q src.ada && $1 $2 > /dev/null 2>../conftest.out"
if AC_TRY_EVAL(ac_try); then
  ifelse([$5], , :, [rm -rf conftest*
  $5])
else
  ifelse([$6], , :, [ rm -rf conftest*
  $6])
fi
rm -f conftest*])

dnl Usage: AM_TRY_ADA_CONFPRAGMA(pragma, success, failure)
dnl Check whether a given configuration pragma is supported.

AC_DEFUN([AM_TRY_ADA_CONFPRAGMA],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AM_TRY_ADA($GNATMAKE_FOR_TARGET,[check.adb],
[procedure Check is begin null; end Check;],[$1],[$2],[$3])])

dnl Usage: AM_PROG_WORKING_ADA
dnl Try to compile a simple Ada program to test the compiler installation
dnl (especially the standard libraries such as Ada.Text_IO)

AC_DEFUN([AM_PROG_WORKING_ADA],
[AC_REQUIRE([AM_PROG_ADA])
AC_REQUIRE([AM_PROG_GNATCHOP])
AC_REQUIRE([AM_PROG_GNATLS])
AC_MSG_CHECKING([if the$crossflagmsg Ada compiler works])
AM_TRY_ADA([$ADA -c],[check.adb],
[with Ada.Text_IO;
procedure Check is
begin
   null;
end Check;
], [], [AC_MSG_RESULT(yes)],
[AC_MSG_RESULT(no)
AC_MSG_ERROR([Ada compiler is not working])])])

dnl Usage: AM_ADA_PREREQ(date, version)
dnl Check that GNAT is at least as recent as date (YYMMDD)

AC_DEFUN([AM_ADA_PREREQ],
[AC_REQUIRE([AM_PROG_WORKING_ADA])
AC_CHECK_PROG(SED, sed, sed)
AC_MSG_CHECKING([if the Ada compiler is recent enough])
am_gnatls_date=`$GNATLS -v | $SED -ne 's/^GNATLS .*(\(.*\)).*$/\1/p'`
if test "$1" -le "$am_gnatls_date"; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  am_gnatls_version=`$GNATLS -v | $SED -ne 's/^GNATLS \(.*\) (.*.*$/\1/p'`
  AC_MSG_ERROR([Please get a version of GNAT no older than [$2 ($1)]
(it looks like you only have GNAT [$am_gnatls_version ($am_gnatls_date)])])
fi])

dnl Usage: AM_CROSS_PROG_ADA
dnl Look for an Ada compiler for the target (same as the host one if host and
dnl target are equal)

AC_DEFUN([AM_CROSS_PROG_ADA],
[AC_BEFORE([$0], [AM_TRY_CROSS_ADA])
AC_REQUIRE([AM_PROG_WORKING_ADA])
 if test $host = $target; then
   ADA_FOR_TARGET=$ADA
   AC_SUBST(ADA_FOR_TARGET)
 else
   AC_CHECK_PROGS(ADA_FOR_TARGET, [$target_alias-$ADA $target-$ADA])
 fi
])

dnl Usage: AM_CROSS_PROG_WORKING_ADA
dnl Try to use Ada compiler for the target if it is different from the host

AC_DEFUN([AM_CROSS_PROG_WORKING_ADA],
[AC_REQUIRE([AM_CROSS_PROG_ADA])
 if test $host != $target; then
   OLDADA=$ADA
   ADA=$ADA_FOR_TARGET
   crossflagmsg=" cross"
   AM_PROG_WORKING_ADA
   crossflagmsg=""
   ADA=$OLDADA
 fi
])

dnl Usage: AM_PROG_GNATMAKE
dnl Look for an Ada make

AC_DEFUN([AM_PROG_GNATMAKE],
[AC_REQUIRE([AC_PROG_CC])
AC_CHECK_PROGS(GNATMAKE, gnatmake)])

dnl Usage: AM_CROSS_PROG_GNATMAKE
dnl Look for gnatmake for the target (same as the host one if host and
dnl target are equal)

AC_DEFUN([AM_CROSS_PROG_GNATMAKE],
[AC_REQUIRE([AM_PROG_WORKING_ADA])
 if test $host = $target; then
   GNATMAKE_FOR_TARGET=$GNATMAKE
   AC_SUBST(GNATMAKE_FOR_TARGET)
 else
   AC_CHECK_PROGS(GNATMAKE_FOR_TARGET, [$target_alias-$GNATMAKE $target-$GNATMAKE])
 fi
])

dnl Usage: AM_CROSS_PROG_GNATLS
dnl Look for gnatls for the target (same as the host one if host and
dnl target are equal)

AC_DEFUN([AM_CROSS_PROG_GNATLS],
[AC_REQUIRE([AM_PROG_WORKING_ADA])
 if test $host = $target; then
   GNATLS_FOR_TARGET=$GNATLS
   AC_SUBST(GNATLS_FOR_TARGET)
 else
   AC_CHECK_PROGS(GNATLS_FOR_TARGET, [$target_alias-$GNATLS $target-$GNATLS])
 fi
])

dnl Usage: AM_CROSS_PROG_CC
dnl Look for CC for the target (same as the host one if host and
dnl target are equal)

AC_DEFUN([AM_CROSS_PROG_CC],
[AC_REQUIRE([AC_PROG_CC])
 if test $host = $target; then
   CC_FOR_TARGET=$CC
   AC_SUBST(CC_FOR_TARGET)
 else
   AC_CHECK_PROGS(CC_FOR_TARGET, [$target_alias-$CC $target-$CC])
 fi
])

dnl Usage: AM_HAS_GNAT_SOCKETS_COPY
dnl Determine whether GNAT.Sockets has a Copy operation.

AC_DEFUN([AM_HAS_GNAT_SOCKETS_COPY],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_BEFORE([AM_HAS_GNAT_SOCKETS_COPY])
AC_BEFORE([AM_HAS_GNAT_OS_LIB_CLOSE_WITH_STATUS])
AC_BEFORE([AM_HAS_PRAGMA_PROFILE_RAVENSCAR])
AC_BEFORE([AM_HAS_PRAGMA_PROFILE_WARNINGS])
AC_MSG_CHECKING([whether you have GNAT.Sockets.Copy])
AM_TRY_ADA($GNATMAKE_FOR_TARGET,[check.adb],
[with GNAT.Sockets;
procedure Check is
   S1, S2 : GNAT.Sockets.Socket_Set_Type;
begin
   GNAT.Sockets.Copy (S1, S2);
end Check;
], [], [AC_MSG_RESULT(yes)
MISS_GNAT_SOCKETS_COPY="--  "],
[AC_MSG_RESULT(no)
HAVE_GNAT_SOCKETS_COPY="--  "])
AC_SUBST(MISS_GNAT_SOCKETS_COPY)dnl
AC_SUBST(HAVE_GNAT_SOCKETS_COPY)])

dnl Usage: AM_HAS_GNAT_OS_LIB_CLOSE_WITH_STATUS
dnl Determine whether GNAT.OS_Lib has a Close operation with status report.

AC_DEFUN([AM_HAS_GNAT_OS_LIB_CLOSE_WITH_STATUS],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether you have GNAT.OS_Lib.Close (FD : File_Descriptor; Status : out Boolean)])
AM_TRY_ADA($GNATMAKE_FOR_TARGET,[check.adb],
[with GNAT.OS_Lib;
procedure Check is
   FD : GNAT.OS_Lib.File_Descriptor;
   Status : boolean;
begin
   GNAT.OS_Lib.Close (FD, Status);
end Check;
], [], [AC_MSG_RESULT(yes)
MISS_GNAT_OS_LIB_CLOSE_WITH_STATUS="--  "],
[AC_MSG_RESULT(no)
HAVE_GNAT_OS_LIB_CLOSE_WITH_STATUS="--  "])
AC_SUBST(MISS_GNAT_OS_LIB_CLOSE_WITH_STATUS)dnl
AC_SUBST(HAVE_GNAT_OS_LIB_CLOSE_WITH_STATUS)])

dnl Usage: AM_HAS_GNAT_PERFECT_HASH_GENERATORS
dnl Determine whether GNAT.Perfect_Hash_Generators exists

AC_DEFUN([AM_HAS_GNAT_PERFECT_HASH_GENERATORS],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether you have GNAT.Perfect_Hash_Generators])
AM_TRY_ADA($GNATMAKE_FOR_TARGET,[check.adb],
[with GNAT.Perfect_Hash_Generators;
procedure Check is begin null; end Check;
], [], [AC_MSG_RESULT(yes)
GNAT_PERFECT_HASH_GENERATORS="GNAT.Perfect_Hash_Generators"],
[AC_MSG_RESULT(no)
GNAT_PERFECT_HASH_GENERATORS="GNAT.Perfect_Hash.Generators"])
AC_SUBST(GNAT_PERFECT_HASH_GENERATORS)])

dnl Usage: AM_HAS_PRAGMA_PROFILE_RAVENSCAR
dnl Test whether pragma Profile (Ravenscar) is supported (if not we use
dnl pragma Ravenscar).

AC_DEFUN([AM_HAS_PRAGMA_PROFILE_RAVENSCAR],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether pragma Profile (Ravenscar) is supported])
AM_TRY_ADA_CONFPRAGMA([pragma Profile (Ravenscar);],
[AC_MSG_RESULT(yes)
PRAGMA_PROFILE_RAVENSCAR="pragma Profile (Ravenscar);"],
[AC_MSG_RESULT(no)
PRAGMA_PROFILE_RAVENSCAR="pragma Ravenscar;"])
AC_SUBST(PRAGMA_PROFILE_RAVENSCAR)])

dnl Usage: AM_HAS_PRAGMA_PROFILE_WARNINGS
dnl Test whether pragma Profile_Warnings (Ravenscar) is supported.

AC_DEFUN([AM_HAS_PRAGMA_PROFILE_WARNINGS],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether pragma Profile_Warnings (Ravenscar) is supported])
AM_TRY_ADA_CONFPRAGMA([pragma Profile_Warnings (Ravenscar);],
[AC_MSG_RESULT(yes)
DISABLE_PROFILE_WARNINGS=""],
[AC_MSG_RESULT(no)
DISABLE_PROFILE_WARNINGS="--  "])
AC_SUBST(DISABLE_PROFILE_WARNINGS)])

dnl Usage: AM_HAS_PRAGMA_SUPPRESS_VALIDITY_CHECK
dnl WAG:5.04
dnl Determine whether pragma Suppress (Validity_Check) can be used to
dnl disable validity checks. If not, we use pragma Suppress (Range_Check)
dnl instead.

AC_DEFUN([AM_HAS_PRAGMA_SUPPRESS_VALIDITY_CHECK],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether pragma Suppress (Validity_Check) is supported])
AM_TRY_ADA_CONFPRAGMA([pragma Suppress (Validity_Check);],
[AC_MSG_RESULT(yes)
SUPPRESS_VALIDITY_USE_VALIDITY=""
SUPPRESS_VALIDITY_USE_RANGE="--  "],
[AC_MSG_RESULT(no)
SUPPRESS_VALIDITY_USE_VALIDITY="--  "
SUPPRESS_VALIDITY_USE_RANGE=""])
AC_SUBST(SUPPRESS_VALIDITY_USE_VALIDITY)
AC_SUBST(SUPPRESS_VALIDITY_USE_RANGE)])

dnl Usage: AM_SUPPORT_RPC_ABORTION
dnl For GNAT5 with ZCX, we cannot support RPC abortion. In this case,
dnl RPC execution may fail even when not aborted. Remove this feature
dnl except when user really wants it to be enabled. When we can provide
dnl this feature with SJLJ exception model and when the user really wants
dnl it, then build GLADE with SJLJ model being the default.

AC_DEFUN([AM_SUPPORT_RPC_ABORTION],
[AC_REQUIRE([AM_CROSS_PROG_GNATLS])
GNAT_RTS_FLAG="";
am_gnat_major_version=`$GNATLS_FOR_TARGET -v | $SED -ne 's/^GNATLS [[^0-9]]*\(.\).*$/\1/p'`
am_system_ads=`$GNATLS_FOR_TARGET -a -s system.ads`
am_gnatlib_dir=`dirname $am_system_ads`
am_gnatlib_dir=`dirname $am_gnatlib_dir`
am_gnat_zcx_by_default=`$SED -ne 's/ZCX_By_Default.*:= *\(.*\);$/\1/p' \
  $am_system_ads`
if test $am_gnat_major_version = "5"; then
  if test $am_gnat_zcx_by_default = "True"; then
    if test $SUPPORT_RPC_ABORTION = "True"; then
      if test -f $am_gnatlib_dir/rts-sjlj/adainclude/system.ads; then
        GNAT_RTS_FLAG="--RTS=rts-sjlj"
        am_gnat_zcx_by_default="False"
      fi
    else
      SUPPORT_RPC_ABORTION="False"
    fi
  else
    SUPPORT_RPC_ABORTION="True"
  fi
else
  SUPPORT_RPC_ABORTION="True"
fi
if test $am_gnat_zcx_by_default = "True"; then
  EXCEPTION_MODEL="zcx"
else
  EXCEPTION_MODEL="sjlj"
fi
])
