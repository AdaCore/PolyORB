dnl IDL preprocessor handling
dnl $Id$

dnl Usage: AM_GCC_XIDL
dnl Test whether the GNAT IDL preprocessor command line switch is available
AC_DEFUN([AM_GCC_XIDL],
[AC_REQUIRE([AM_PROG_ADA])
AC_MSG_CHECKING([if GNAT provides an IDL preprocessor])
mkdir conftest
echo "interface Foo {};" > conftest/input.idl
ac_try="cd conftest && $ADA -E -x idl -o input.I input.idl && cat input.I > /dev/null"
if AC_TRY_EVAL(ac_try); then
  HAVE_GCC_XIDL=yes
else
  HAVE_GCC_XIDL=no
fi
AC_MSG_RESULT($HAVE_GCC_XIDL)
rm -fr conftest])

dnl Usage: AM_PROG_IDLCPP
dnl Find an appropriate IDL preprocessor

AC_DEFUN([AM_PROG_IDLCPP],
[AC_REQUIRE([AM_GCC_XIDL])
AC_MSG_CHECKING([what IDL preprocessor to use])
if test "${IDLCPP+set}" = set; then
  : IDLCPP set by the user

elif test "$HAVE_GCC_XIDL" = yes; then
  # IDLCPP provided by GNAT gcc driver
  IDLCPP="$ADA -E -x idl"

else
  # IDLCPP provided by C++ compiler
  AC_PROG_CXXCPP
  IDLCPP="$CXXCPP"

  case "$CXXCPP" in
    *g++*)
      if test "${CXXCPPFLAGS}" = ""; then
        IDLCPPFLAGS="-x c++ -ansi"
        # Options to use GNU C++ preprocessor as IDL preprocessor
        # -x c++       force C++ preprocessor mode (even though it cannot be
        #              inferred from filename extension .idl)
        # -ansi        disable GCC-specific behaviour
      fi
      ;;
  esac
fi
AC_MSG_RESULT([$IDLCPP $IDLCPPFLAGS])
AC_SUBST(IDLCPP)
AC_SUBST(IDLCPPFLAGS)
])

dnl Usage: AM_IDLCPP_NEEDS_DOT
dnl Check whether it is necessary to add a trailing dot for the C++
dnl preprocessor to create an output file without extension. On Windows,
dnl a default extension ".exe" is appended if no trailing dot is present.
AC_DEFUN([AM_IDLCPP_NEEDS_DOT],
[AC_REQUIRE([AM_PROG_IDLCPP])
AC_MSG_CHECKING([if the IDL preprocessor -o option requires a dot])
mkdir conftest
touch conftest/input
dnl Note: We test for presence of "output.exe", not absence of "output",
dnl becuse as of Cygwin 1.7, if "output.exe" is present and "output" is
dnl missing, then shell tools from coreutils append the .exe extension
dnl automagically, and make it appear as though "output" was present.
ac_try="cd conftest && $IDLCPP $IDLCPPFLAGS -o output input && cat output.exe > /dev/null"
if AC_TRY_EVAL(ac_try); then
  AC_MSG_RESULT(yes)
  AC_SUBST(IDLCPP_OUTPUT_SUFFIX, ".")
else
  AC_MSG_RESULT(no)
  AC_SUBST(IDLCPP_OUTPUT_SUFFIX, "")
fi
rm -fr conftest])
