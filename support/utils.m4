dnl Miscellaneous shell utilities quirks handling
dnl $Id$

dnl Usage: AM_PROG_XARGS_I
dnl Look for proper variant of xargs command line switch (old GNU
dnl findutils wanted -i{}, POSIX and the rest of the world use -I{})

AC_DEFUN([AM_PROG_XARGS_I],
[AC_MSG_CHECKING([if xargs supports POSIX -I option])
if AC_TRY_COMMAND(test `echo foo | xargs -I'{}' echo X'{}'X 2> /dev/null` = XfooX); then
  : POSIXly correct implementation
  AC_MSG_RESULT([yes])
  XARGS_I="xargs -I{}"
else
  : Old GNU findutils
  AC_MSG_RESULT([no (assuming old GNU findutils variant)])
  XARGS_I="xargs -i{}"
fi
AC_SUBST(XARGS_I)
])
