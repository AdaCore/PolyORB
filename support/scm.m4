dnl Source configuration management
dnl $Id$

dnl Usage: AM_SCM
dnl Set CMREVISION from DISTRIB_CMREVISION if provided by support/distrib.m4.
dnl If DISTRIB_CMREVISION is undefined, assume we are configuring from a
dnl Subversion checkout, and use "svn info" to retrieve the values.

AC_DEFUN([AM_SCM],[
  ifdef([DISTRIB_CMREVISION], [CMREVISION=DISTRIB_CMREVISION],[
    AC_CHECK_PROG([GIT],[git],[`which git`])

    if ! test -z $GIT; then
      GIT_HEAD=`cd ${srcdir} && ${GIT} rev-parse --short=8 HEAD`
      if test $? = 0; then
        last_changed_rev="$GIT_HEAD"
      fi
    fi
    CMREVISION=${last_changed_rev:-unknown}
  ])
  AC_SUBST(CMREVISION)
])
