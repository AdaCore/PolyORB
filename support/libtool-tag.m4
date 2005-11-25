dnl Determine whether libtool supports --tag
dnl $Id$

AC_DEFUN([AC_LIBTOOL_HAS_TAG],[
AC_MSG_CHECKING([whether libtool supports --tag])
if grep "[[-]]-tag" $srcdir/support/ltmain.sh > /dev/null; then
  AC_MSG_RESULT([yes])
  LIBTOOL_TAG=--tag=CC
else
  AC_MSG_RESULT([no])
  LIBTOOL_TAG=
fi
AC_SUBST(LIBTOOL_TAG)
])
