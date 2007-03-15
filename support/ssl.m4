AC_DEFUN([AM_WITH_OPENSSL],
[AC_MSG_CHECKING(for OpenSSL library)
AC_ARG_WITH(openssl,
  AC_HELP_STRING([--with-openssl@<:@=ARG@:>@],
		 [enable SSL support, will check ARG/ssl /usr/local/ssl /usr/lib/ssl /usr/ssl
		  /usr/pkg /usr/local /usr]),
[
    AC_MSG_RESULT(yes)
    for dir in $withval /usr/local/ssl /usr/lib/ssl /usr/ssl /usr/pkg /usr/local /usr; do
        ssldir="$dir"
        if test -f "$dir/include/openssl/ssl.h"; then
            AC_MSG_NOTICE([OpenSSL found in $ssldir]);
            CPPFLAGS="$CPPFLAGS -I$ssldir/include -DHAVE_SSL";
            # Special case for RedHat Linux 9
            if test -f /usr/kerberos/include/krb5.h; then
                CPPFLAGS="-I/usr/kerberos/include/ ${CPPFLAGS}"
            fi

            SSL_LDFLAGS="-L$ssldir/lib -lssl -lcrypto"
            # Note that order is important here (-lssl must come before
            # -lcrypto), since some undefined symbols in libssl are provided
            # by libcrypto.

            HAVE_SSL=yes
            break;
        fi
    done

    if test x$HAVE_SSL = xyes; then
        dnl Libcrypto depends on the dynamic linking API: make sure we have
        dnl -ldl in LIBS when required. Most systems provide dlopen (),
        dnl but HP-UX uses shl_load().
        AC_CHECK_LIB(dl, dlopen, [SSL_LDFLAGS="$SSL_LDFLAGS -ldl"], [
          AC_CHECK_LIB(dl, shl_load, [SSL_LDFLAGS="$SSL_LDFLAGS -ldl"])
        ])
    else
        AC_MSG_ERROR(cannot find OpenSSL)
        NO_SSL="--  "
    fi
],
[
    AC_MSG_RESULT(no)
    NO_SSL="--  "
])
AC_SUBST(HAVE_SSL)
AC_SUBST(NO_SSL)

# Convert the space-separated SSL_LDFLAGS into a sequence of string literals
# concatenated with ASCII.Nul separators, which is what prama Linker_Options
# expects.

set_linker_options() {
  while test @S|@# -gt 0; do
    SSL_LINKER_OPTIONS="${SSL_LINKER_OPTIONS}\"@S|@1\""
    if test @S|@# -gt 1; then
      SSL_LINKER_OPTIONS="${SSL_LINKER_OPTIONS} & ASCII.Nul & "
    fi
    shift
  done
  if test -z "${SSL_LINKER_OPTIONS}"; then
    NO_SSL_LINKER_OPTIONS="--  "
  fi
}
set_linker_options $SSL_LDFLAGS
AC_SUBST(SSL_LINKER_OPTIONS)
AC_SUBST(NO_SSL_LINKER_OPTIONS)
])dnl
