dnl Subversion infrastructure
dnl $Id$

dnl Usage: AM_SUBVERSION
dnl Set SVNREVISION, unless previously set in distrib.m4.
dnl If the passed value is "unknown", assume we are configuring from a
dnl Subversion checkout, and use "svn info" to retrieve the values,
dnl else we are configuring from packaged sources, and the variables are
dnl set to the explicitly passed values.

AC_DEFUN([AM_SUBVERSION],[
  ifdef(SVNREVISION, [SVNREVISION]=SVNREVISION,[
    AC_CHECK_PROG([SVN],[svn],[`which svn`])

    if ! test -z $SVN; then
      SVNINFO=`cd ${srcdir} && ${SVN} info .`
      if test $? = 0; then
        last_changed_rev=`echo "$SVNINFO" | sed -n "s/^Last Changed Rev: \(.*\)\$/\1/p"`
      fi
    fi
    [SVNREVISION]=${last_changed_rev:-unknown}
  ])
  AC_MSG_NOTICE([building rev. ${SVNREVISION}])
  AC_SUBST(SVNREVISION)
])
