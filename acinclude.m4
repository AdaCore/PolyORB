dnl Various base M4 macros
dnl $Id$

dnl Allow various parameters to be overridden by distrib.m4, a file
dnl generated when preparing a source distribution package.

m4_sinclude([support/distrib.m4])

dnl Usage: pkg_version(release)
dnl Expands to the complete version string built from:
dnl   - the indicated product release;
dnl   - ADDITIONAL_VERSION
dnl ADDITIONAL_VERSION may be defined in distrib.m4.

define([pkg_version],[$1[]ifdef([ADDITIONAL_VERSION],ADDITIONAL_VERSION)])

dnl Usage: FILTER_OUTPUT_FILES(list, files)
dnl If LIST is the name of a file containing a list of filenames
dnl return those members of list FILES whose corresponding templates (XXX.in)
dnl are mentioned in LIST. Otherwise return FILES unchanged.

dnl AC_DEFUN([FILTER_OUTPUT_FILES],
dnl [ifdef($1,
dnl   [m4_foreach_w(mfile, m4_normalize(defn($1)), [define([$M$]mfile,1)])
dnl    m4_foreach_w(file, m4_normalize($2), [ifdef([$M$]file[.in],[file ])])],
dnl   $2)])
AC_DEFUN([FILTER_OUTPUT_FILES],[esyscmd(
[if test -f $1; then
  mkdir conftest
  sort < $1 | uniq | sed -n 's/\.in$//p' > conftest/list
  echo "$2" | sort | uniq > conftest/files
  comm -13 conftest/list conftest/files
  rm -fr conftest
else
  echo "$2"
fi])])
