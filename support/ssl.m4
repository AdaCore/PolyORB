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
            found_ssl="yes";
            CPPFLAGS="$CPPFLAGS -I$ssldir/include -DHAVE_SSL";
            # Special case for RedHat Linux 9
            if test -f /usr/kerberos/include/krb5.h; then
                CPPFLAGS="-I/usr/kerberos/include/ ${CPPFLAGS}"
            fi
            LDFLAGS="$LDFLAGS -L$ssldir/lib";
            #LIBS="$LIBS -lssl -lcrypto";
            HAVE_SSL=yes
            break;
        fi
    done
    if test x$found_ssl != xyes; then
        AC_MSG_ERROR(Cannot find OpenSSL)
    fi
    AC_SUBST(HAVE_SSL)
],
[
    AC_MSG_RESULT(no)
])
])dnl
