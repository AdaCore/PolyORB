dnl aclocal.m4 generated automatically by aclocal 1.4

dnl Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AC_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" && test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package]))
AC_REQUIRE([AM_SANITY_CHECK])
AC_REQUIRE([AC_ARG_PROGRAM])
dnl FIXME This is truly gross.
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_REQUIRE([AC_PROG_MAKE_SET])])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "[$]*" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   if test "[$]*" != "X $srcdir/configure conftestfile" \
      && test "[$]*" != "X conftestfile $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN(AM_MISSING_PROG,
[AC_MSG_CHECKING(for working $2)
# Run test in a subshell; some versions of sh will print an error if
# an executable is not found, even if stderr is redirected.
# Redirect stdin to placate older versions of autoconf.  Sigh.
if ($2 --version) < /dev/null > /dev/null 2>&1; then
   $1=$2
   AC_MSG_RESULT(found)
else
   $1="$3/missing $2"
   AC_MSG_RESULT(missing)
fi
AC_SUBST($1)])


dnl Usage: AM_PROG_ADA
dnl Look for an Ada compiler (ADA environment variable, then gcc, then $CC)

AC_DEFUN(AM_PROG_ADA,
[AC_BEFORE([$0], [AM_TRY_ADA])
AC_REQUIRE([AC_PROG_CC])
AC_CHECK_PROGS(ADA, adagcc gnatgcc gcc)
if test -z "$ADA"; then
  AC_MSG_RESULT([  Tentatively using $CC as an Ada compiler])
  ADA="$CC"
fi])

dnl Usage: AM_TRY_ADA(filename, content, success, failure)
dnl Compile an Ada program and report its success or failure

AC_DEFUN(AM_TRY_ADA,
[AC_REQUIRE([AM_PROG_ADA])
mkdir conftest
cat > conftest/[$1] <<EOF
[$2]
EOF
ac_try="cd conftest && $ADA -c $1 > /dev/null 2>../conftest.out"
if AC_TRY_EVAL(ac_try); then
  ifelse([$3], , :, [rm -rf conftest*
  $3])
else
  ifelse([$4], , :, [ rm -rf conftest*
  $4])
fi
rm -f conftest*])

dnl Usage: AM_PROG_WORKING_ADA
dnl Try to compile a simple Ada program to test the compiler installation
dnl (especially the standard libraries such as Ada.Text_IO)

AC_DEFUN(AM_PROG_WORKING_ADA,
[AC_REQUIRE([AM_PROG_ADA])
AC_MSG_CHECKING([if the$crossflagmsg Ada compiler works])
AM_TRY_ADA([check.adb],
[with Ada.Text_IO;
procedure Check is
begin
   null;
end Check;
], [AC_MSG_RESULT(yes)],
[AC_MSG_RESULT(no)
AC_MSG_ERROR([Ada compiler is not working])])])

dnl Usage: AM_ADA_PREREQ(date, version)
dnl Check that GNAT is at least as recent as date (YYMMDD)

AC_DEFUN(AM_ADA_PREREQ,
[AC_REQUIRE([AM_PROG_WORKING_ADA])
AC_CHECK_PROG(GNATLS, gnatls, gnatls)
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

AC_DEFUN(AM_CROSS_PROG_ADA,
[AC_REQUIRE([AM_PROG_WORKING_ADA])
 if test $host = $target; then
   ADA_FOR_TARGET=$ADA
   AC_SUBST(ADA_FOR_TARGET)
 else
   AC_CHECK_PROGS(ADA_FOR_TARGET, [$target_alias-$ADA $target-$ADA])
 fi
])

dnl Usage: AM_CROSS_PROG_WORKING_ADA
dnl Try to use Ada compiler for the target if it is different from the host

AC_DEFUN(AM_CROSS_PROG_WORKING_ADA,
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

AC_DEFUN([AC_GNAT_SOURCE],
[
  AC_MSG_CHECKING([for GNAT sources])
  if test -f gnat/osint.ads; then
    GNAT_SOURCE=../gnat
    AC_MSG_RESULT(gnat)
  elif test -f ada/osint.ads; then
    GNAT_SOURCE=../ada
    AC_MSG_RESULT(ada)
  elif test -f $srcdir/gnat/osint.ads; then
    GNAT_SOURCE=[\${top_srcdir}/gnat]
    AC_MSG_RESULT($srcdir/gnat)
  elif test -f $srcdir/ada/osint.ads; then
    GNAT_SOURCE=[\${top_srcdir}/ada]
    AC_MSG_RESULT($srcdir/ada)
  else
    AC_MSG_ERROR([no sources found])
  fi
  AC_SUBST(GNAT_SOURCE)
])

