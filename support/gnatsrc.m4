## Find GNAT source
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
