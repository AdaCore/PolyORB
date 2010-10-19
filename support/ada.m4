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
dnl The ":" lines below ensure that neither branch of the "if" is empty
if AC_TRY_COMMAND([cd conftest && $GNATCHOP -q src.ada && $1 $2 > /dev/null 2>../conftest.out])
then
  : Success
  $5
else
  : Failure
  $6
fi
rm -fr conftest*])

dnl Usage: AM_TRY_ADA_CONFPRAGMA(pragma, success, failure)
dnl Check whether a given configuration pragma is supported.

AC_DEFUN([AM_TRY_ADA_CONFPRAGMA],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AM_TRY_ADA($GNATMAKE_FOR_TARGET,[check.adb],
[procedure Check is begin null; end Check;],[$1],[$2],[$3])])

dnl Usage: AM_TRY_ADA_COMPILER_SWITCH(switch, success, failure)
dnl Check whether a given compiler command line switch is supported.

AC_DEFUN([AM_TRY_ADA_COMPILER_SWITCH],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AM_TRY_ADA([$GNATMAKE_FOR_TARGET $1],[check.adb],
[procedure Check is begin null; end Check;],[],[$2],[$3])])

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
dnl target are equal). Sets GNATMAKE_FOR_TARGET and GNAT_DRIVER_FOR_TARGET.

AC_DEFUN([AM_CROSS_PROG_GNATMAKE],
[AC_REQUIRE([AM_PROG_WORKING_ADA])
 if test $host = $target; then
   GNATMAKE_FOR_TARGET=$GNATMAKE
   AC_SUBST(GNATMAKE_FOR_TARGET)
 else
   AC_CHECK_PROGS(GNATMAKE_FOR_TARGET, [$target_alias-$GNATMAKE $target-$GNATMAKE])
 fi
 GNAT_DRIVER_FOR_TARGET=`echo $GNATMAKE_FOR_TARGET | sed 's/make$//'`
 AC_SUBST(GNAT_DRIVER_FOR_TARGET)

 AC_MSG_CHECKING([whether $GNATMAKE_FOR_TARGET supports -aPdir])
 if AC_TRY_COMMAND([gnatmake 2>&1 | grep " -aPdir" > /dev/null])
 then
   HAVE_GNATMAKE_APDIR=yes
 else
   HAVE_GNATMAKE_APDIR=no
 fi
 AC_MSG_RESULT($HAVE_GNATMAKE_APDIR)
 AC_SUBST(HAVE_GNATMAKE_APDIR)
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

dnl Usage: AM_HAS_GNAT_PROJECT(project)
dnl Check whether a given project file is available, and set
dnl HAVE_GNAT_PROJECT_<project> to "yes" or "no" accordingly.

AC_DEFUN([AM_HAS_GNAT_PROJECT],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether GNAT project $1.gpr is available])
mkdir conftest
cat > conftest/check.gpr <<EOF
with "[$1]";
project Check is for Source_Files use (); end Check;
EOF
if AC_TRY_COMMAND([cd conftest && $GNAT_DRIVER_FOR_TARGET ls -Pcheck system.ads > /dev/null 2>../conftest.out])
then
  HAVE_GNAT_PROJECT_$1=yes
else
  HAVE_GNAT_PROJECT_$1=no
fi
AC_MSG_RESULT($HAVE_GNAT_PROJECT_$1)
AC_SUBST(HAVE_GNAT_PROJECT_$1)
rm -fr conftest])

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

dnl Usage: AM_HAS_STYLESW_YG
dnl Test whether the style checking switch -gnatyg (apply GNAT style checks)
dnl is supported.

AC_DEFUN([AM_HAS_STYLESW_YG],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether GNAT style checks are available])
AM_TRY_ADA_COMPILER_SWITCH([-gnatyg],
[AC_MSG_RESULT(yes)
STYLE_SWITCH="-gnatyg"],
[AC_MSG_RESULT(no, falling back to -gnaty)
STYLE_SWITCH="-gnaty"])
AC_SUBST(STYLE_SWITCH)])

dnl Usage: AM_SUPPORT_RPC_ABORTION
dnl For GNAT 5 or later with ZCX, we cannot support RPC abortion. In this
dbl case, RPC execution may fail even when not aborted. Remove this feature
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
if test -z "$am_gnat_zcx_by_default"; then
  am_gnat_zcx_by_default=False
fi
if test $am_gnat_major_version -ge "5"; then
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

dnl Usage: AM_ARG_ENABLE_POLICY(what, default)
dnl Allow user to set configuration pragmas Assertion_Policy and Debug_Policy.
dnl The provided default value may be overridden by earlier configure.ac
dnl macros by setting xxx_POLICY_DEFAULT.

define([downcase], [translit([$1], [A-Z], [a-z])])
define([upcase], [translit([$1], [a-z], [A-Z])])

AC_DEFUN([AM_ARG_ENABLE_POLICY],
[
define([_argname],downcase($1)[-policy])
define([_varname],upcase($1)[_POLICY])
define([_defname],upcase($1)[_POLICY_DEFAULT])
_varname=${_defname:=$2}
AC_ARG_ENABLE(_argname,
AS_HELP_STRING([--enable-]_argname[=(Check|Ignore)], [Set ]$1[ policy @<:@default=$2@:>@]),
[
  case "`echo "${enableval}" | tr A-Z a-z`" in
    yes|check) _varname=Check ;;
    no|ignore) _varname=Ignore ;;
    *) AC_MSG_ERROR("Invalid $1 policy identifier: ${enableval}") ;;
  esac
])
AC_SUBST(_varname)
undefine([_argname])
undefine([_varname])
undefine([_defname])
])

dnl Usage: AM_HAS_ATOMIC_INCDEC32
dnl Determine whether platform/GNAT supports atomic increment/decrement
dnl operations

AC_DEFUN([AM_HAS_INTRINSIC_SYNC_COUNTERS],
[AC_REQUIRE([AM_CROSS_PROG_GNATMAKE])
AC_MSG_CHECKING([whether platform supports atomic increment/decrement])
AM_TRY_ADA([$GNATMAKE_FOR_TARGET $ADAFLAGS_FOR_TARGET],[check.adb],
[
with Interfaces; use Interfaces;
procedure Check is
   function Sync_Add_And_Fetch
     (Ptr   : access Interfaces.Integer_32;
      Value : Interfaces.Integer_32) return Interfaces.Integer_32;
   pragma Import (Intrinsic, Sync_Add_And_Fetch, "__sync_add_and_fetch_4");
   X : aliased Interfaces.Integer_32;
   Y : Interfaces.Integer_32 := 0;
   pragma Volatile (Y);
   --  On some platforms (e.g. i386), GCC has limited support for
   --  __sync_add_and_fetch_4 for the case where the result is not used.
   --  Here we want to test for general availability, so make Y volatile to
   --  prevent the store operation from being discarded.
begin
   Y := Sync_Add_And_Fetch (X'Access, 1);
end Check;
], [], [AC_MSG_RESULT(yes)
SYNC_COUNTERS_IMPL="intrinsic"],
[AC_MSG_RESULT(no)
SYNC_COUNTERS_IMPL="mutex"])
AC_SUBST(SYNC_COUNTERS_IMPL)])
