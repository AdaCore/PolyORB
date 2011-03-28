## Ada compiler handling

dnl Usage: AM_PROG_GNAT
dnl Look for an Ada compiler (gnatmake)

AC_DEFUN([AM_PROG_GNAT],
[AC_BEFORE([$0], [AM_TRY_GNAT])
AC_CHECK_PROGS(GNAT, gnatmake)
])

dnl Usage: AM_TRY_GNAT(filename, content, success, failure)
dnl Compile an Ada program and report its success or failure

AC_DEFUN([AM_TRY_GNAT],
[AC_REQUIRE([AM_PROG_GNAT])
mkdir conftest
cat > conftest/[$1] <<EOF
[$2]
EOF
ac_try="cd conftest && $GNAT -c $1 > /dev/null 2>../conftest.out"
if AC_TRY_EVAL(ac_try); then
  ifelse([$3], , :, [rm -rf conftest*
  $3])
else
  ifelse([$4], , :, [ rm -rf conftest*
  $4])
fi
rm -f conftest*])

dnl Usage: AM_PROG_GNAT_FOR_HOST
dnl Try to compile a simple Ada program to test the compiler installation
dnl (especially the standard libraries such as Ada.Text_IO)

AC_DEFUN([AM_PROG_GNAT_FOR_HOST],
[AC_REQUIRE([AM_PROG_GNAT])
AC_MSG_CHECKING([if the Ada compiler works])
AM_TRY_GNAT([check.adb],
[with Ada.Text_IO;
procedure Check is
begin
   null;
end Check;
], [
 AC_MSG_RESULT(yes)
 GNAT_FOR_HOST=gnatmake
],
[
  AC_MSG_RESULT(no)
AC_MSG_ERROR([Ada compiler is not working])])])

dnl Usage: AM_GNAT_PREREQ(date, version)
dnl Check that GNAT is at least as recent as date (YYMMDD)

AC_DEFUN([AM_GNAT_PREREQ],
[AC_REQUIRE([AM_PROG_GNAT_FOR_HOST])
AC_CHECK_PROG(GNATLS, gnatls, gnatls)
AC_CHECK_PROG(SED, sed, sed)
AC_MSG_CHECKING([if the Ada compiler is recent enough])
am_gnatls_date=`$GNATLS -v | $SED -ne 's/^GNATLS .*(\(........\).*$/\1/p'`
if test "$1" -le "$am_gnatls_date"; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
  am_gnatls_version=`$GNATLS -v | $SED -ne 's/^GNATLS \(.*\) (.*.*$/\1/p'`
  AC_MSG_ERROR([Please get a version of GNAT no older than [$2 ($1)]
(it looks like you only have GNAT [$am_gnatls_version ($am_gnatls_date)])])
fi])

dnl Usage: AM_PROG_GNAT_FOR_TARGET
dnl Look for an Ada compiler for the target (same as the host one if host and
dnl target are equal)

AC_DEFUN([AM_PROG_GNAT_FOR_TARGET],
[AC_REQUIRE([AM_PROG_GNAT_FOR_HOST])
 if test $host = $target; then
   GNAT_FOR_TARGET=$GNAT_FOR_HOST
   AC_SUBST(GNAT_FOR_TARGET)
 else
   AC_CHECK_PROGS(GNAT_FOR_TARGET,
     [$target_alias-$GNAT_FOR_HOST $target-$GNAT_FOR_HOST])
 fi
])

dnl Usage: AM_SUPPORT_RPC_ABORTION
dnl For GNAT 5 or later with ZCX, we cannot support RPC abortion. In this
dbl case, RPC execution may fail even when not aborted. Remove this feature
dnl except when user really wants it to be enabled. When we can provide
dnl this feature with SJLJ exception model and when the user really wants
dnl it, then build GLADE with SJLJ model being the default.

AC_DEFUN([AM_SUPPORT_RPC_ABORTION],
[AC_REQUIRE([AM_PROG_GNAT_FOR_HOST])
AC_CHECK_PROG(GNATLS, gnatls, gnatls)
GNAT_RTS_FLAG="";
am_gnat_major_version=`$GNATLS -v | $SED -ne 's/^GNATLS [[^0-9]]*\(.\).*$/\1/p'`
am_system_ads=`$GNATLS -a -s system.ads`
am_gnatlib_dir=`dirname $am_system_ads`
am_gnatlib_dir=`dirname $am_gnatlib_dir`
am_gnat_zcx_by_default=`$SED -ne 's/ZCX_By_Default.*:= *\(.*\);$/\1/p' \
  $am_system_ads`
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

