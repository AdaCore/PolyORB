# Automake macro for PolyORB.
#
# Contributed to PolyORB by Vadim Godunko <vgodunko@rost.ru>   
# See README for more details

dnl ##########################################################################
dnl AM_PATH_POLYORB([MINIMUM-VERSION [, ACTION-IF-FOUND
dnl                    [, [ACTION-IF-NOT-FOUND]]])
dnl Look for PolyORB, then define POLYORB_ADAFLAGS, POLYORB_LIBS and
dnl POLYORB_IDLFLAGS

AC_DEFUN([AM_PATH_POLYORB],
[
AC_ARG_WITH(polyorb-prefix,
  AC_HELP_STRING([--with-polyorb-prefix=PREFIX],
                 [Prefix where PolyORB is installed (optional)]),
  [polyorb_config_prefix="$withval"], [polyorb_config_prefix=""])

min_polyorb_version=ifelse([$1], , 1.2, $1)
min_polyorb_major_version=`echo $min_polyorb_version | \
  sed 's/\([[0-9]]*\).\([[0-9]]*\)/\1/'`
min_polyorb_minor_version=`echo $min_polyorb_version | \
  sed 's/\([[0-9]]*\).\([[0-9]]*\)/\2/'`

AS_IF([test x$polyorb_config_prefix != x],
      [POLYORB_CONFIG=$polyorb_config_prefix/bin/polyorb-config])

AC_PATH_PROG(POLYORB_CONFIG, polyorb-config, no)

AS_IF([test "$POLYORB_CONFIG" != "no"],
      [AC_MSG_CHECKING([for PolyORB - version >= $min_polyorb_version])
       POLYORB_ADAFLAGS=`$POLYORB_CONFIG --cflags`
       POLYORB_LIBS=`$POLYORB_CONFIG --libs`
       POLYORB_IDLFLAGS=`$POLYORB_CONFIG --idls`
       polyorb_major_version=`$POLYORB_CONFIG --version | \
         sed 's/PolyORB \([[0-9]]*\).\([[0-9]]*\).*/\1/'`
       polyorb_minor_version=`$POLYORB_CONFIG --version | \
         sed 's/PolyORB \([[0-9]]*\).\([[0-9]]*\).*/\2/'`
       AS_IF([test $polyorb_major_version -gt $min_polyorb_major_version -o \( $polyorb_major_version -eq $min_polyorb_major_version -a $polyorb_minor_version -ge $min_polyorb_minor_version \)],
              [AC_MSG_RESULT(yes)
               ifelse([$2], , :, [$2])],
              [AC_MSG_RESULT(no)
               ifelse([$3], , :, [$3])])],
      [$3])
AC_SUBST(POLYORB_ADAFLAGS)
AC_SUBST(POLYORB_LIBS)
AC_SUBST(POLYORB_IDLFLAGS)
])
