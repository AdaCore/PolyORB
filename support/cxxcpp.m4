dnl Ada compiler handling
dnl $Id$

dnl Usage: AM_CXXCPP_NEEDS_DOT
dnl Check whether it is necessary to add a trailing dot for the
dnl C++ preprocessor to create an output file without extension.
AC_DEFUN([AM_CXXCPP_NEEDS_DOT],
[AC_REQUIRE([AC_PROG_CXXCPP])
AC_MSG_CHECKING([if the C++ preprocessor -o option requires a dot])
mkdir conftest
touch conftest/input
ac_try="cd conftest && $CXXCPP $CXXCPPFLAGS -o output input && cat output > /dev/null"
if AC_TRY_EVAL(ac_try); then
  AC_MSG_RESULT(no)
  AC_SUBST(CXXCPP_OUTPUT_SUFFIX, "")
else
  AC_MSG_RESULT(yes)
  AC_SUBST(CXXCPP_OUTPUT_SUFFIX, ".")
fi
rm -fr conftest])
