dnl Subversion infrastructure
dnl $Id: ada.m4 124902 2008-05-21 09:51:07Z quinot $

dnl Usage: AM_SUBVERSION(svnrevision)
dnl Set SVNREVISION.
dnl If the passed value is "unknown", assume we are configuring from a
dnl Subversion checkout, and use "svn info" to retrieve the values,
dnl else we are configuring from packaged sources, and the variables are
dnl set to the explicitly passed values.

AC_DEFUN([AM_SUBVERSION],[
  SVNREVISION="$1"

  if test "$SVNREVISION" = "unknown"
  then
    AC_CHECK_PROG([SVN],[svn],[`which svn`])

    if ! test -z $SVN; then
      SVNINFO=`cd ${srcdir} && ${SVN} info .`
      if test $? = 0; then
        SVNREVISION=`echo "$SVNINFO" | grep '^Revision' | cut -f 2- -d ' '`
        # SVNDATE=`echo $SVNINFO | grep 'Last Changed Date' | cut -f 4-6 -d ' '`
      fi
    fi
  fi
  AC_SUBST(SVNREVISION)
])
